use std::{ops::RangeInclusive, sync::Arc};

use la::LapackeFunctionsStatic;
use nalgebra::DVector;

use crate::integrators::{
    Integrator,
    interpolation::{ApplyFunc, Interpolation},
};

pub struct NaturalCubicIntegrator {
    range: RangeInclusive<f64>,
    widths: Arc<[f64]>,

    d: DVector<f64>,
    du: DVector<f64>,
    dl: DVector<f64>,

    lapacke: LapackeFunctionsStatic,
}

impl Integrator for NaturalCubicIntegrator {
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.interpolation_mapped(ys, map).integral(epsilon)
    }
    fn integrate_interest_points(&self, ys: &[f64], interest_points: &[f64], epsilon: f64) -> f64 {
        self.interpolation(ys)
            .integral_interest_points(interest_points, epsilon)
    }
    fn interpolation_mapped<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(&[f64], &mut [f64]) + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation_mapped(ys, map)))
    }
    fn integrate_mapped_interest_points(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        interest_points: &[f64],
        epsilon: f64,
    ) -> f64 {
        self.interpolation_mapped(ys, map)
            .integral_interest_points(interest_points, epsilon)
    }
    fn integrate(&self, ys: &[f64], _epsilon: f64) -> f64 {
        self.interpolation(ys).integral()
    }
    fn integrate_len(&self) -> usize {
        self.widths.len() + 1
    }
    fn interpolation<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation(ys)))
    }
}

impl NaturalCubicIntegrator {
    pub fn new(xs: &[f64], lapacke: LapackeFunctionsStatic) -> Self {
        let range = xs.first().cloned().unwrap_or(0.0)..=xs.last().cloned().unwrap_or(0.0);
        let widths: Arc<[f64]> =
            Vec::from_iter(xs.windows(2).map(|window| window[1] - window[0])).into();
        let n_func = widths.len();
        let d = DVector::<f64>::from_vec(
            (0..n_func - 1)
                .map(|i| 4.0 * (1.0 / (widths[i + 1] * widths[i]) + 1.0 / (widths[i] * widths[i])))
                .collect::<Vec<_>>(),
        );
        let du = DVector::<f64>::from_vec(
            (0..n_func - 2)
                .map(|i| 2.0 / (widths[i + 1] * widths[i + 1]))
                .collect::<Vec<_>>(),
        );
        let dl = DVector::<f64>::from_vec(
            (1..n_func - 1)
                .map(|i| 2.0 / (widths[i] * widths[i - 1]))
                .collect::<Vec<_>>(),
        );

        Self {
            range,
            widths,
            d,
            du,
            dl,

            lapacke,
        }
    }

    fn interpolation_mapped<Y, M>(
        &self,
        ys: Y,
        map: M,
    ) -> ApplyFunc<NaturalCubicInterpolation<Y>, M>
    where
        Y: AsRef<[f64]>,
        M: Fn(&[f64], &mut [f64]) + Send + Sync,
    {
        ApplyFunc::new(self.interpolation(ys), map)
    }

    pub fn interpolation<Y>(&self, ys: Y) -> NaturalCubicInterpolation<Y>
    where
        Y: AsRef<[f64]>,
    {
        assert_eq!(ys.as_ref().len(), self.widths.len() + 1);

        let mut d = self.d.clone();
        let mut du = self.du.clone();
        let mut dl = self.dl.clone();

        let mut b = DVector::from_vec(
            (0..self.widths.len() - 1)
                .map(|i| {
                    let ys = ys.as_ref();
                    6.0 * (ys[i + 2] / (self.widths[i + 1] * self.widths[i + 1])
                        - ys[i] / (self.widths[i] * self.widths[i])
                        + ys[i + 1]
                            * (1.0 / (self.widths[i] * self.widths[i])
                                - 1.0 / (self.widths[i + 1] * self.widths[i + 1])))
                })
                .collect(),
        );

        self.lapacke
            .dgtsv(&mut d, &mut du, &mut dl, &mut b)
            .unwrap();

        NaturalCubicInterpolation {
            range: self.range.clone(),
            widths: self.widths.clone(),
            c0_buf: ys,
            c3_buf: b,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NaturalCubicInterpolation<Y>
where
    Y: AsRef<[f64]>,
{
    widths: Arc<[f64]>,
    range: RangeInclusive<f64>,
    c0_buf: Y,
    c3_buf: DVector<f64>,
}

impl<Y> Interpolation for NaturalCubicInterpolation<Y>
where
    Y: AsRef<[f64]> + Send + Sync,
{
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.range.clone()
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        self.eval_many(xs, ys_out);
        Ok(ys_out)
    }
}

impl<Y> NaturalCubicInterpolation<Y>
where
    Y: AsRef<[f64]>,
{
    pub fn integral(&self) -> f64 {
        (0..self.widths.as_ref().len())
            .map(|idx| {
                let (c0, c1, c2, c3) = self.coefficients(idx);
                (c0 / 2.0 + c1 / 2.0 + c2 / 12.0 - c3 / 12.0) * self.widths[idx]
            })
            .sum()
    }

    pub fn coefficients(&self, idx: usize) -> (f64, f64, f64, f64) {
        let widths = self.widths.as_ref();
        assert!(idx < widths.len());
        let c0_buf = self.c0_buf.as_ref();
        let c3_buf = self.c3_buf.as_slice();
        let (c0, c1, c2, c3);
        c0 = c0_buf[idx];
        c1 = c0_buf[idx + 1];
        if idx == 0 {
            c3 = c3_buf[0];
            c2 = (-3.0 * c0 + 3.0 * c1 - c3) / 2.0;
            // c2 = 0.0;
        } else if idx == widths.len() - 1 {
            c2 = c3_buf[widths.len() - 2] * widths[widths.len() - 1] / widths[widths.len() - 2];
            c3 = (-3.0 * c0 + 3.0 * c1 - c2) / 2.0;
            // c3 = 0.0;
        } else {
            c2 = c3_buf[idx - 1] * widths[idx] / widths[idx - 1];
            c3 = c3_buf[idx];
        }

        (c0, c1, c2, c3)
    }
    ///
    pub fn eval_many(&self, xs: &[f64], ys_out: &mut [f64]) {
        let widths = self.widths.as_ref();
        let c0_buf = self.c0_buf.as_ref();
        let mut current_x = *self.range.start();
        let mut idx = 0;

        for (x, y) in xs.iter().zip(ys_out.iter_mut()) {
            let mut relative_x = *x - current_x;

            while idx < widths.len() && relative_x > widths[idx] {
                current_x += widths[idx];
                relative_x -= widths[idx];
                idx += 1;
            }

            if relative_x < 0.0 || idx >= widths.len() {
                *y = c0_buf[idx];
            } else {
                let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
                let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
                let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
                let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };
                let (c0, c1, c2, c3) = self.coefficients(idx);
                let x_scaled = relative_x / self.widths[idx];

                assert!((0.0..=1.0).contains(&x_scaled));

                *y = c0 * h00(x_scaled)
                    + c1 * h01(x_scaled)
                    + c2 * h10(x_scaled)
                    + c3 * h11(x_scaled);
            }
        }
    }

    pub fn eval(&self, x: f64) -> f64 {
        let widths = self.widths.as_ref();
        let c0_buf = self.c0_buf.as_ref();
        let mut relative_x = x - self.range.start();

        let mut idx = 0;

        while idx < widths.len() && relative_x > widths[idx] {
            relative_x -= widths[idx];
            idx += 1;
        }

        if relative_x < 0.0 || idx >= widths.len() {
            c0_buf[idx]
        } else {
            let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
            let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
            let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
            let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };
            let (c0, c1, c2, c3) = self.coefficients(idx);
            let x_scaled = relative_x / widths[idx];

            assert!((0.0..=1.0).contains(&x_scaled));

            c0 * h00(x_scaled) + c1 * h01(x_scaled) + c2 * h10(x_scaled) + c3 * h11(x_scaled)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use la::{BlasLib, LapackeLib};

    use crate::integrators::{Integrator, Interpolation, NaturalCubicIntegrator};

    #[test]
    fn natural_cubic_interp() {
        let test_func = f64::sin;
        let xs_vec = Vec::from_iter((0..=100).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let blas_lib = BlasLib::new().unwrap();
        let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();

        let integrator = NaturalCubicIntegrator::new(&xs_vec, lapacke_lib.functions_static());

        let interpolation = integrator.interpolation(&ys_vec);

        let mut many_result = vec![0.0; test_xs_vec.len()];
        interpolation
            .interpolate(&test_xs_vec, &mut many_result)
            .unwrap();

        for (idx, x) in test_xs_vec.iter().enumerate() {
            let answer = test_func(*x);

            let (result_one, result_many) = (interpolation.eval(*x), many_result[idx]);

            // Test equivalence
            assert!(
                (result_one - result_many).abs() < 100.0 * f64::EPSILON,
                "Equivalent results should be achieved but are different, {} vs {} at x={}",
                result_one,
                result_many,
                x,
            );

            // Test accuracy
            assert!(
                (result_one - answer).abs() <= 8e-3,
                "Accuracy insufficient, {} vs {}, difference {} at x={}",
                result_one,
                answer,
                result_one - answer,
                x
            );
        }
    }

    #[test]
    fn natural_cubic_integrate() {
        let tests: &[(&str, &dyn Fn(f64) -> f64, f64)] = &[
            // Polynomials
            ("1", &|_x: f64| 1.0, 1.0),
            ("x", &|x: f64| x, 1.0 / 2.0),
            ("x^2", &|x: f64| x * x, 1.0 / 3.0),
            ("x^3", &|x: f64| x * x * x, 1.0 / 4.0),
            ("x^4", &|x: f64| x.powi(4), 1.0 / 5.0),
            ("x^5", &|x: f64| x.powi(5), 1.0 / 6.0),
            ("x^6", &|x: f64| x.powi(6), 1.0 / 7.0),
            ("x^7", &|x: f64| x.powi(7), 1.0 / 8.0),
            // Reciprocal
            ("1/(x + 1)", &|x: f64| 1.0 / (x + 1.0), 2.0f64.ln()),
            ("1/(x + 1)^2", &|x: f64| 1.0 / (x + 1.0).powi(2), 1.0 / 2.0),
            ("1/(x + 1)^3", &|x: f64| 1.0 / (x + 1.0).powi(3), 3.0 / 8.0),
            ("1/(x + 1)^4", &|x: f64| 1.0 / (x + 1.0).powi(4), 7.0 / 24.0),
            // ln(x)/x tail
            (
                "ln(100x + 10)/(100x + 10)",
                &|x: f64| (100.0 * x + 10.0).ln() / (100.0 * x + 10.0),
                0.0839630877936,
            ),
            // Exponential tail
            (
                "10x e^{-10x}",
                &|x: f64| (10.0 * x) * (-10.0 * x).exp(),
                0.0999500600773,
            ),
            // Sinusoidal
            ("sin(10x)", &|x: f64| (10.0 * x).sin(), 0.459697694132),
            // Quarter circle
            (
                "quarter circle",
                &|x: f64| (1.0 - x * x).sqrt(),
                f64::consts::PI / 4.0,
            ),
        ];

        let xs_vec = Vec::from_iter((0..=100).map(|i| match i {
            0 => 0.0,
            100 => 1.0,
            _ => (i as f64 + 0.1 * (i as f64 * 1430.1).sin()) / 100.0,
        }));
        let blas_lib = BlasLib::new().unwrap();
        let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();

        let integrator = NaturalCubicIntegrator::new(&xs_vec, lapacke_lib.functions_static());

        for &(name, test_func, answer) in tests.iter() {
            let ys_vec = Vec::from_iter(xs_vec.iter().cloned().map(test_func));
            let result = integrator.integrate(&ys_vec, 1e-6);
            let difference = result - answer;
            let difference_fraction = difference / answer;

            assert!(
                difference < 1e-5,
                "Insufficient integral accuracy for `{}`, {} vs {}, difference {} = {}%",
                name,
                result,
                answer,
                difference,
                difference_fraction * 100.0
            );
        }
    }
}
