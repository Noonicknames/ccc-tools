use std::ops::{Range, RangeInclusive};

use la::LapackeFunctionsStatic;
use nalgebra::{DMatrix, DVector};
use serde::{Deserialize, Serialize};

use crate::integrate::math::{Grid, moment_fitted};

pub mod math;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IntegrationKind {
    #[default]
    MonotoneCubic,
    NaturalCubic,
    AutoGauss,
    Gauss,
}

pub trait Integrator {
    fn integrate(&mut self, ys: &[f64]) -> f64;
    fn integrate_len(&self) -> usize;

    /// Return an interpolation implementing [Interpolation], if supported.
    fn interpolate<'a>(&'a mut self, ys: &'a [f64]) -> Option<Box<dyn Interpolation + 'a>> {
        _ = ys;
        None
    }
}

pub trait Interpolation {
    fn interpolate(&mut self, xs: &mut [f64]) -> Result<(), ()>;
}

pub struct SubIntegrators {
    partial: Vec<(Range<usize>, Box<dyn Integrator>)>,
}

impl SubIntegrators {
    pub fn new() -> Self {
        Self {
            partial: Vec::new(),
        }
    }

    pub fn push(&mut self, range: Range<usize>, partial_integrator: impl Integrator + 'static) {
        self.partial.push((range, Box::new(partial_integrator)));
    }

    pub fn push_boxed(&mut self, range: Range<usize>, partial_integrator: Box<dyn Integrator>) {
        self.partial.push((range, partial_integrator));
    }

    pub fn interpolation<'a>(&'a mut self, ys: &'a [f64]) -> SubInterpolations<'a> {}
}

impl Integrator for SubIntegrators {
    fn integrate(&mut self, ys: &[f64]) -> f64 {
        self.partial
            .iter_mut()
            .map(|(range, integrator)| integrator.integrate(&ys[range.clone()]))
            .sum()
    }
    fn integrate_len(&self) -> usize {
        if self.partial.len() == 0 {
            return 0;
        }
        let (start, end) = self.partial.iter().fold(
            (self.partial[0].0.start, self.partial[0].0.end),
            |(start, end), (range, _)| (start.min(range.start), end.max(range.end)),
        );

        end - start
    }
    fn interpolate(&mut self, ys: &[f64]) -> Option<&dyn Interpolation> {}
}

pub struct LinearInterpolation<'a> {
    x_vec: &'a [f64],
    y_vec: &'a [f64],
}

impl<'a> LinearInterpolation<'a> {
    pub fn new(x_vec: &'a [f64], y_vec: &'a [f64]) -> Self {
        assert_eq!(x_vec.len(), y_vec.len());
        Self { x_vec, y_vec }
    }
    pub fn eval(&self, x: f64) -> f64 {
        if self.x_vec.is_empty() {
            0.0
        } else if x < self.x_vec[0] {
            self.y_vec[0]
        } else if x > self.x_vec[self.x_vec.len() - 1] {
            self.y_vec[self.x_vec.len() - 1]
        } else {
            match self.x_vec.binary_search_by(|this_x| this_x.total_cmp(&x)) {
                Ok(idx) => self.y_vec[idx],
                Err(idx) => {
                    if idx >= self.x_vec.len() - 1 {
                        return self.y_vec[self.y_vec.len() - 1];   
                    }
                    let t = (x - self.x_vec[idx]) / self.x_vec[idx + 1];

                    t * self.y_vec[idx]
                        + (1.0 - t) * self.y_vec[idx + 1]
                }
            }
        }
    }

    pub fn eval_many(&self, xs: &mut [f64]) {

    }
}

impl<'a> Interpolation for LinearInterpolation<'a> {
    fn interpolate(&mut self, xs: &mut [f64]) -> Result<(), ()> {
        Ok(())
    }
}

pub struct SubInterpolations<'a> {
    partials: Vec<(Range<usize>, &'a dyn Interpolation)>,
}

impl<'a> SubInterpolations<'a> {}

pub struct MonotoneCubicIntegrator {
    range: RangeInclusive<f64>,
    widths: DVector<f64>,

    m_buf: DVector<f64>,
    delta_buf: DVector<f64>,
}

impl Integrator for MonotoneCubicIntegrator {
    fn integrate(&mut self, ys: &[f64]) -> f64 {
        self.interpolation(ys).integral()
    }
    fn integrate_len(&self) -> usize {
        self.widths.len() + 1
    }
}

impl MonotoneCubicIntegrator {
    pub fn new(xs: &[f64]) -> Self {
        let range = xs.first().cloned().unwrap_or(0.0)..=xs.last().cloned().unwrap_or(0.0);
        let widths = DVector::from_vec(xs.windows(2).map(|window| window[1] - window[0]).collect());
        let delta_buf = DVector::zeros(widths.len());
        let m_buf = DVector::zeros(xs.len());

        Self {
            range,
            widths,

            m_buf,
            delta_buf,
        }
    }

    pub fn interpolation<'a>(&'a mut self, ys: &'a [f64]) -> MonotoneCubicInterpolation<'a> {
        assert_eq!(ys.len(), self.widths.len() + 1);

        for ((y_window, width), delta) in ys
            .windows(2)
            .zip(self.widths.iter())
            .zip(self.delta_buf.iter_mut())
        {
            *delta = (y_window[1] - y_window[0]) / width;
        }

        self.m_buf[0] = self.delta_buf[0];
        *self.m_buf.as_mut_slice().last_mut().unwrap() = self.delta_buf[self.delta_buf.len() - 1];

        for (i, delta_window) in (1..self.m_buf.len() - 1).zip(self.delta_buf.as_slice().windows(2))
        {
            if delta_window[0] * delta_window[1] <= f64::EPSILON {
                self.m_buf[i] = 0.0;
            } else {
                self.m_buf[i] = (delta_window[0] + delta_window[1]) / 2.0;
            }
        }

        // m_vec.last_mut().iter_mut().for_each(|last| **last = 0.0);

        for i in 0..self.delta_buf.len() {
            if self.delta_buf[i] == 0.0 {
                self.m_buf[i] = 0.0;
                self.m_buf[i + 1] = 0.0;
                continue;
            } else {
                let alpha = self.m_buf[i] / self.delta_buf[i];
                let beta = if i > 0 {
                    self.m_buf[i] / self.delta_buf[i - 1]
                } else {
                    0.0
                };

                if alpha < 0.0 || beta < 0.0 {
                    self.m_buf[i] = 0.0;
                }

                let norm_sq = alpha * alpha + beta * beta;
                if norm_sq > 9.0 {
                    let tau = 3.0 / norm_sq.sqrt();
                    self.m_buf[i] = tau * alpha * self.delta_buf[i];
                    self.m_buf[i + 1] = tau * beta * self.delta_buf[i];
                }
            }
        }

        MonotoneCubicInterpolation {
            range: self.range.clone(),
            y_vec: ys,
            widths: &self.widths,
            m_buf: &self.m_buf,
        }
    }
}

pub struct MonotoneCubicInterpolation<'a> {
    range: RangeInclusive<f64>,
    y_vec: &'a [f64],
    widths: &'a DVector<f64>,
    m_buf: &'a DVector<f64>,
}

impl<'a> MonotoneCubicInterpolation<'a> {
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
        let (c0, c1, c2, c3);
        c0 = self.y_vec[idx];
        c1 = self.y_vec[idx + 1];
        c2 = self.m_buf[idx] * self.widths[idx];
        c3 = self.m_buf[idx + 1] * self.widths[idx];

        (c0, c1, c2, c3)
    }
    ///
    pub fn eval_many(&self, xs: &mut [f64]) {
        let mut current_x = *self.range.start();
        let mut idx = 0;

        for x in xs.iter_mut() {
            let mut relative_x = *x - current_x;

            while idx < self.widths.len() && relative_x > self.widths[idx] {
                current_x += self.widths[idx];
                relative_x -= self.widths[idx];
                idx += 1;
            }

            if relative_x < 0.0 || idx >= self.widths.len() {
                *x = self.y_vec[idx];
            } else {
                let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
                let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
                let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
                let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };
                let (c0, c1, c2, c3) = self.coefficients(idx);
                let x_scaled = relative_x / self.widths[idx];

                assert!((0.0..=1.0).contains(&x_scaled));

                *x = c0 * h00(x_scaled)
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
            self.y_vec[idx]
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

impl<'a> Interpolation for MonotoneCubicInterpolation<'a> {
    fn interpolate(&mut self, xs: &mut [f64]) -> Result<(), ()> {
        self.eval_many(xs);
        Ok(())
    }
}

pub struct NaturalCubicIntegrator {
    range: RangeInclusive<f64>,
    widths: Vec<f64>,

    d: DVector<f64>,
    du: DVector<f64>,
    dl: DVector<f64>,

    d_buf: DVector<f64>,
    du_buf: DVector<f64>,
    dl_buf: DVector<f64>,
    b_buf: DVector<f64>,

    lapacke: LapackeFunctionsStatic,
}

impl Integrator for NaturalCubicIntegrator {
    fn integrate(&mut self, ys: &[f64]) -> f64 {
        self.interpolation(ys).integral()
    }
    fn integrate_len(&self) -> usize {
        self.widths.len() + 1
    }
}

impl NaturalCubicIntegrator {
    pub fn new(xs: &[f64], lapacke: LapackeFunctionsStatic) -> Self {
        let range = xs.first().cloned().unwrap_or(0.0)..=xs.last().cloned().unwrap_or(0.0);
        let widths = Vec::from_iter(xs.windows(2).map(|window| window[1] - window[0]));
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

        let d_buf = DVector::zeros(n_func - 1);
        let du_buf = DVector::zeros(n_func - 2);
        let dl_buf = DVector::zeros(n_func - 2);
        let b_buf = DVector::zeros(n_func - 1);

        Self {
            range,
            widths,
            d,
            du,
            dl,

            d_buf,
            du_buf,
            dl_buf,
            b_buf,

            lapacke,
        }
    }

    pub fn interpolation<'a>(&'a mut self, ys: &'a [f64]) -> NaturalCubicInterpolation<'a> {
        assert_eq!(ys.len(), self.widths.len() + 1);
        self.d_buf.copy_from(&self.d);
        self.du_buf.copy_from(&self.du);
        self.dl_buf.copy_from(&self.dl);

        self.b_buf
            .iter_mut()
            .zip(0..self.widths.len() - 1)
            .for_each(|(b, i)| {
                *b = 6.0
                    * (ys[i + 2] / (self.widths[i + 1] * self.widths[i + 1])
                        - ys[i] / (self.widths[i] * self.widths[i])
                        + ys[i + 1]
                            * (1.0 / (self.widths[i] * self.widths[i])
                                - 1.0 / (self.widths[i + 1] * self.widths[i + 1])));
            });

        self.lapacke
            .dgtsv(
                &mut self.d_buf,
                &mut self.du_buf,
                &mut self.dl_buf,
                &mut self.b_buf,
            )
            .unwrap();

        NaturalCubicInterpolation {
            range: self.range.clone(),
            widths: self.widths.as_slice(),
            c0_buf: ys,
            c3_buf: self.b_buf.as_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NaturalCubicInterpolation<'a> {
    widths: &'a [f64],
    range: RangeInclusive<f64>,
    c0_buf: &'a [f64],
    c3_buf: &'a [f64],
}

impl<'a> Interpolation for NaturalCubicInterpolation<'a> {
    fn interpolate(&mut self, xs: &mut [f64]) -> Result<(), ()> {
        self.eval_many(xs);
        Ok(())
    }
}

impl<'a> NaturalCubicInterpolation<'a> {
    pub fn integral(&self) -> f64 {
        (0..self.widths.len())
            .map(|idx| {
                let (c0, c1, c2, c3) = self.coefficients(idx);
                (c0 / 2.0 + c1 / 2.0 + c2 / 12.0 - c3 / 12.0) * self.widths[idx]
            })
            .sum()
    }

    pub fn coefficients(&self, idx: usize) -> (f64, f64, f64, f64) {
        assert!(idx < self.widths.len());
        let (c0, c1, c2, c3);
        c0 = self.c0_buf[idx];
        c1 = self.c0_buf[idx + 1];
        if idx == 0 {
            c3 = self.c3_buf[0];
            c2 = (-3.0 * c0 + 3.0 * c1 - c3) / 2.0;
            // c2 = 0.0;
        } else if idx == self.widths.len() - 1 {
            c2 = self.c3_buf[self.widths.len() - 2] * self.widths[self.widths.len() - 1]
                / self.widths[self.widths.len() - 2];
            c3 = (-3.0 * c0 + 3.0 * c1 - c2) / 2.0;
            // c3 = 0.0;
        } else {
            c2 = self.c3_buf[idx - 1] * self.widths[idx] / self.widths[idx - 1];
            c3 = self.c3_buf[idx];
        }

        (c0, c1, c2, c3)
    }
    ///
    pub fn eval_many(&self, xs: &mut [f64]) {
        let mut current_x = *self.range.start();
        let mut idx = 0;

        for x in xs.iter_mut() {
            let mut relative_x = *x - current_x;

            while idx < self.widths.len() && relative_x > self.widths[idx] {
                current_x += self.widths[idx];
                relative_x -= self.widths[idx];
                idx += 1;
            }

            if relative_x < 0.0 || idx >= self.widths.len() {
                *x = self.c0_buf[idx];
            } else {
                let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
                let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
                let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
                let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };
                let (c0, c1, c2, c3) = self.coefficients(idx);
                let x_scaled = relative_x / self.widths[idx];

                assert!((0.0..=1.0).contains(&x_scaled));

                *x = c0 * h00(x_scaled)
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
            self.c0_buf[idx]
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

pub struct GaussIntegrator {
    grid: Grid<f64>,
}

impl Integrator for GaussIntegrator {
    fn integrate(&mut self, ys: &[f64]) -> f64 {
        self.grid.eval(ys)
    }
    fn integrate_len(&self) -> usize {
        self.grid.weights.len()
    }
}

impl GaussIntegrator {
    pub fn new(xs: &[f64]) -> Self {
        if xs.len() < 2 {
            return Self {
                grid: Grid {
                    points: DMatrix::zeros(0, 0),
                    weights: DVector::zeros(0),
                },
            };
        }
        let x_points_temp = DVector::from_vec(xs.to_vec());
        let start = *xs.first().unwrap();
        let x_range = *xs.last().unwrap() - start;

        let scaled_x_points = &x_points_temp.add_scalar(-start) / x_range;
        let mut grid = moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);
        grid.scale_x(x_range);
        grid.translate(&DVector::from_vec(vec![start])).unwrap();

        Self { grid }
    }

    pub fn is_empty(&self) -> bool {
        self.grid.points.is_empty()
    }
    pub fn auto(
        x_points: &[f64],
        integrand: &[f64],
        mut error_threshold: impl FnMut(usize) -> f64,
        max_degree: usize,
    ) -> SubIntegrators {
        let mut result = SubIntegrators::new();

        let get_result = |istart: usize, iend: usize, x_points: &[f64]| -> (GaussIntegrator, f64) {
            let mut grid = GaussIntegrator::new(&x_points[istart..=iend]);
            let val = grid.integrate(&integrand[istart..=iend]);
            (grid, val)
        };

        let mut prev_idx = 0;

        'outer: loop {
            let istart = prev_idx;
            if istart == x_points.len() - 1 {
                break 'outer;
            }
            let max_degree = max_degree.min(x_points.len() - istart - 1);
            let (mut prev_integrator, mut prev_val) = get_result(istart, istart + 1, x_points);

            for degree in 2..=max_degree {
                let (integrator, val) = get_result(istart, istart + degree, x_points);

                // Predicted difference, for a safer integration against resonances.
                let predicted_diff = get_result(istart + degree - 1, istart + degree, x_points).1;
                let actual_diff = val - prev_val;
                let error_percent = actual_diff / predicted_diff * 100.0 - 100.0;

                if error_percent.abs() > error_threshold(degree) || prev_val < 0.0 {
                    result.push(istart..istart + degree, prev_integrator);
                    prev_idx = istart + degree - 1;
                    continue 'outer;
                }

                prev_integrator = integrator;
                prev_val = val;
            }

            result.push(istart..istart + max_degree + 1, prev_integrator);
            prev_idx = istart + max_degree;
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use la::{BlasLib, LapackeLib};

    use crate::integrate::{Integrator, MonotoneCubicIntegrator, NaturalCubicIntegrator};

    #[test]
    fn natural_cubic_interp() {
        let test_func = f64::sin;
        let xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let blas_lib = BlasLib::new().unwrap();
        let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();

        let mut integrator = NaturalCubicIntegrator::new(&xs_vec, lapacke_lib.functions_static());

        let interpolation = integrator.interpolation(&ys_vec);

        let mut many_result = test_xs_vec.clone();
        interpolation.eval_many(&mut many_result);

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

        let mut integrator = NaturalCubicIntegrator::new(&xs_vec, lapacke_lib.functions_static());

        for &(name, test_func, answer) in tests.iter() {
            let ys_vec = Vec::from_iter(xs_vec.iter().cloned().map(test_func));
            let result = integrator.integrate(&ys_vec);
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

    #[test]
    fn monotone_cubic_interp() {
        let test_func = f64::sin;
        let xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let mut integrator = MonotoneCubicIntegrator::new(&xs_vec);

        let interpolation = integrator.interpolation(&ys_vec);

        let mut many_result = test_xs_vec.clone();
        interpolation.eval_many(&mut many_result);

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

        let mut integrator = MonotoneCubicIntegrator::new(&xs_vec);

        for &(name, test_func, answer) in tests.iter() {
            let ys_vec = Vec::from_iter(xs_vec.iter().cloned().map(test_func));
            let result = integrator.integrate(&ys_vec);
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
