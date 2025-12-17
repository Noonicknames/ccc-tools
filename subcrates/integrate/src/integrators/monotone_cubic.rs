use std::{ops::RangeInclusive, sync::Arc};

use crate::integrators::{
    Integrator,
    interpolation::{ApplyFunc, Interpolation},
};

pub struct MonotoneCubicIntegrator {
    range: RangeInclusive<f64>,
    widths: Arc<[f64]>,
    // m_buf: DVector<f64>,
    // delta_buf: DVector<f64>,
}

impl Integrator for MonotoneCubicIntegrator {
    fn integrate(&self, ys: &[f64], _epsilon: f64) -> f64 {
        self.interpolation(ys).integral()
    }
    fn integrate_len(&self) -> usize {
        self.widths.len() + 1
    }
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.interpolation_mapped(ys, map).integral(epsilon)
    }
    fn interpolation_mapped<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(&[f64], &mut [f64]) + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(ApplyFunc::new(self.interpolation(ys), map)))
    }
    fn interpolation<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation(ys)))
    }
}

impl MonotoneCubicIntegrator {
    pub fn new(xs: &[f64]) -> Self {
        let range = xs.first().cloned().unwrap_or(0.0)..=xs.last().cloned().unwrap_or(0.0);
        let widths = xs.windows(2).map(|window| window[1] - window[0]).collect();

        Self { range, widths }
    }

    fn interpolation_mapped<Y, M>(
        &self,
        ys: Y,
        map: M,
    ) -> ApplyFunc<MonotoneCubicInterpolation<Y>, M>
    where
        Y: AsRef<[f64]>,
        M: Fn(&[f64], &mut [f64]) + Send + Sync,
    {
        ApplyFunc::new(self.interpolation(ys), map)
    }

    pub fn interpolation<Y>(&self, ys: Y) -> MonotoneCubicInterpolation<Y>
    where
        Y: AsRef<[f64]>,
    {
        assert_eq!(ys.as_ref().len(), self.widths.len() + 1);

        let mut delta_buf = vec![0.0; self.widths.len()];
        let mut m_buf = vec![0.0; self.widths.len() + 1];

        {
            let ys = ys.as_ref();

            for ((y_window, width), delta) in ys
                .windows(2)
                .zip(self.widths.iter())
                .zip(delta_buf.iter_mut())
            {
                *delta = (y_window[1] - y_window[0]) / width;
            }

            m_buf[0] = delta_buf[0];
            *m_buf.as_mut_slice().last_mut().unwrap() = delta_buf[delta_buf.len() - 1];

            for (i, delta_window) in (1..m_buf.len() - 1).zip(delta_buf.as_slice().windows(2)) {
                if delta_window[0] * delta_window[1] <= f64::EPSILON {
                    m_buf[i] = 0.0;
                } else {
                    m_buf[i] = (delta_window[0] + delta_window[1]) / 2.0;
                }
            }

            // m_vec.last_mut().iter_mut().for_each(|last| **last = 0.0);

            for i in 0..delta_buf.len() {
                if delta_buf[i] == 0.0 {
                    m_buf[i] = 0.0;
                    m_buf[i + 1] = 0.0;
                    continue;
                } else {
                    let alpha = m_buf[i] / delta_buf[i];
                    let beta = if i > 0 {
                        m_buf[i] / delta_buf[i - 1]
                    } else {
                        0.0
                    };

                    if alpha < 0.0 || beta < 0.0 {
                        m_buf[i] = 0.0;
                    }

                    let norm_sq = alpha * alpha + beta * beta;
                    if norm_sq > 9.0 {
                        let tau = 3.0 / norm_sq.sqrt();
                        m_buf[i] = tau * alpha * delta_buf[i];
                        m_buf[i + 1] = tau * beta * delta_buf[i];
                    }
                }
            }
        }

        MonotoneCubicInterpolation {
            range: self.range.clone(),
            y_vec: ys,
            widths: Arc::clone(&self.widths),
            m_buf: m_buf,
        }
    }
}

pub struct MonotoneCubicInterpolation<Y>
where
    Y: AsRef<[f64]>,
{
    range: RangeInclusive<f64>,
    y_vec: Y,
    widths: Arc<[f64]>,
    m_buf: Vec<f64>,
}

impl<Y> MonotoneCubicInterpolation<Y>
where
    Y: AsRef<[f64]>,
{
    pub fn integral(&self) -> f64 {
        (0..self.widths.len())
            .map(|idx| {
                let (c0, c1, c2, c3) = self.coefficients(idx);
                (c0 / 2.0 + c1 / 2.0 + c2 / 12.0 - c3 / 12.0) * self.widths[idx]
            })
            .sum()
    }

    pub fn coefficients(&self, idx: usize) -> (f64, f64, f64, f64) {
        assert!(idx < self.m_buf.len() - 1);
        let y_vec = self.y_vec.as_ref();
        let (c0, c1, c2, c3);
        c0 = y_vec[idx];
        c1 = y_vec[idx + 1];
        c2 = self.m_buf[idx] * self.widths[idx];
        c3 = self.m_buf[idx + 1] * self.widths[idx];

        (c0, c1, c2, c3)
    }

    ///
    pub fn eval_many(&self, xs: &[f64], ys_out: &mut [f64]) {
        let mut current_x = *self.range.start();
        let mut idx = 0;

        for (x, y) in xs.iter().zip(ys_out.iter_mut()) {
            let mut relative_x = *x - current_x;

            while idx < self.widths.len() && relative_x > self.widths[idx] {
                current_x += self.widths[idx];
                relative_x -= self.widths[idx];
                idx += 1;
            }

            if relative_x < 0.0 || idx >= self.widths.len() {
                *y = self.y_vec.as_ref()[idx];
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
        let mut relative_x = x - self.range.start();

        let mut idx = 0;

        while idx < self.widths.len() && relative_x > self.widths[idx] {
            relative_x -= self.widths[idx];
            idx += 1;
        }

        if relative_x < 0.0 || idx >= self.widths.len() {
            self.y_vec.as_ref()[idx]
        } else {
            let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
            let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
            let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
            let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };
            let (c0, c1, c2, c3) = self.coefficients(idx);
            let x_scaled = relative_x / self.widths[idx];

            assert!((0.0..=1.0).contains(&x_scaled));

            c0 * h00(x_scaled) + c1 * h01(x_scaled) + c2 * h10(x_scaled) + c3 * h11(x_scaled)
        }
    }
}

impl<Y> Interpolation for MonotoneCubicInterpolation<Y>
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

#[cfg(test)]
mod tests {
    use std::f64;

    use crate::integrators::{Integrator, Interpolation, MonotoneCubicIntegrator};

    #[test]
    fn monotone_cubic_interp() {
        let test_func = f64::sin;
        let xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let integrator = MonotoneCubicIntegrator::new(&xs_vec);

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
                (result_one - answer).abs() <= 5e-3,
                "Accuracy insufficient, {} vs {}, difference {} at x={}",
                result_one,
                answer,
                result_one - answer,
                x
            );
        }
    }

    #[test]
    fn monotone_cubic_integrate() {
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

        let integrator = MonotoneCubicIntegrator::new(&xs_vec);

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
