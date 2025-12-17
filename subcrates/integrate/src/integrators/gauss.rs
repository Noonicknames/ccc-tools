use nalgebra::{DMatrix, DVector};

use crate::{
    integrators::{Integrator, SubIntegrators},
    math::{Grid, moment_fitted},
};

pub struct GaussIntegrator {
    grid: Grid<f64>,
}

impl Integrator for GaussIntegrator {
    fn integrate_interest_points(&self, ys: &[f64], interest_points: &[f64], epsilon: f64) -> f64 {
        _ = interest_points;
        self.integrate(ys, epsilon)
    }
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        _epsilon: f64,
    ) -> f64 {
        let mut ys = ys.to_vec();

        (map)(self.grid.points.as_slice(), &mut ys);
        self.grid.eval(&ys)
    }
    fn integrate_mapped_interest_points(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        interest_points: &[f64],
        epsilon: f64,
    ) -> f64 {
        _ = interest_points;
        self.integrate_mapped(ys, map, epsilon)
    }
    fn integrate(&self, ys: &[f64], _epsilon: f64) -> f64 {
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
            let grid = GaussIntegrator::new(&x_points[istart..=iend]);
            let val = grid.integrate(&integrand[istart..=iend], 0.0); // epsilon isd not used in gauss 
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
