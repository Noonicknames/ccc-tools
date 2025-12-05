use std::{
    ops::{Range, RangeInclusive},
    sync::Arc,
};

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

pub trait Integrator: Send + Sync {
    fn integrate(&self, ys: &[f64], epsilon: f64) -> f64;

    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(f64, f64) -> f64 + Send + Sync),
        epsilon: f64,
    ) -> f64;
    fn integrate_len(&self) -> usize;

    fn interpolation_mapped<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(f64, f64) -> f64 + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        assert_eq!(xs.len(), ys.len());
        _ = (xs, ys, map);
        None
    }
    /// Return an interpolation implementing [Interpolation], if supported.
    fn interpolation<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
    ) -> Option<Box<dyn Interpolation + 'a>> {
        assert_eq!(xs.len(), ys.len());
        _ = (xs, ys);
        None
    }
}

fn kahan_sum(nums: impl Iterator<Item = f64>) -> f64 {
    let mut sum = 0.0;
    let mut c = 0.0;

    nums.for_each(|num| {
        let y = num - c;
        let t = sum + y;

        c = (t - sum) - y;

        sum = t;
    });

    sum
}

pub trait Interpolation: Send + Sync {
    fn interpolate_range(&self) -> RangeInclusive<f64>;
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()>;
    fn integral(&self, epsilon: f64) -> f64 {
        let interpolate_range = self.interpolate_range();

        let mut intervals = 128;

        let xs = Vec::from_iter((0..=intervals).map(|i| {
            let t = i as f64 / intervals as f64;
            t * interpolate_range.end() + (1.0 - t) * interpolate_range.start()
        }));

        let mut ys_buf = vec![0.0; xs.len()];

        let mut prev_answer = {
            let step = (interpolate_range.end() - interpolate_range.start()) / intervals as f64;
            let ys = self
                .interpolate(&xs, &mut ys_buf)
                .expect("All xs were within the interpolation range, should not error.");

            let mut result = 0.0;

            // Boole's rule
            result += 7.0 * (ys[0] + ys[intervals]);

            result += 32.0 * kahan_sum(ys.iter().skip(1).step_by(2).cloned());
            result += 12.0 * kahan_sum(ys.iter().skip(2).step_by(4).cloned());
            result += 14.0 * kahan_sum(ys.iter().skip(4).step_by(4).cloned());

            result = result * step * 2.0 / 45.0;

            result
        };

        loop {
            let new_xs = Vec::from_iter((0..intervals).map(|i| {
                let t = (2 * i + 1) as f64 / (2*intervals) as f64;
                t * interpolate_range.end() + (1.0 - t) * interpolate_range.start()
            }));

            intervals *= 2;

            let mut new_ys_buf = vec![0.0; new_xs.len()];

            let new_ys = self
                .interpolate(&new_xs, &mut new_ys_buf)
                .expect("All xs were within interpolation range, should not error.");

            assert_eq!(ys_buf.len(), new_ys.len() + 1);

            ys_buf = ys_buf
                .iter()
                .zip(new_ys.iter())
                .flat_map(|(&x, &y)| [x, y])
                .chain(ys_buf.last().cloned())
                .collect::<Vec<f64>>();

            let current_answer = {
                let ys = &ys_buf;
                let step = (interpolate_range.end() - interpolate_range.start()) / intervals as f64;

                let mut result = 0.0;

                // Boole's rule
                result += 7.0 * (ys[0] + ys[intervals]);

                result += 32.0 * kahan_sum(ys.iter().skip(1).step_by(2).cloned());
                result += 12.0 * kahan_sum(ys.iter().skip(2).step_by(4).cloned());
                result += 14.0 * kahan_sum(ys.iter().skip(4).step_by(4).cloned());

                result = result * step * 2.0 / 45.0;

                result
            };

            if (current_answer - prev_answer).abs() < epsilon {
                break current_answer;
            } else {
                prev_answer = current_answer;
            }
        }
    }
}

pub struct ApplyFunc<I, F>
where
    F: Fn(f64, f64) -> f64 + Sync + Send,
{
    inner: I,
    func: F,
}

impl<I, F> ApplyFunc<I, F>
where
    F: Fn(f64, f64) -> f64 + Sync + Send,
{
    pub fn new(inner: I, func: F) -> Self {
        Self { inner, func }
    }
}

impl<I, F> Interpolation for ApplyFunc<I, F>
where
    I: Interpolation,
    F: Fn(f64, f64) -> f64 + Sync + Send,
{
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.inner.interpolate_range()
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let ys = self.inner.interpolate(xs, ys_out)?;
        ys.iter_mut()
            .zip(xs.iter())
            .for_each(|(y, &x)| *y = (self.func)(x, *y));
        Ok(ys)
    }
}

impl<F> Interpolation for ApplyFunc<Box<dyn Interpolation>, F>
where
    F: Fn(f64, f64) -> f64 + Sync + Send,
{
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.inner.interpolate_range()
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let ys = self.inner.interpolate(xs, ys_out)?;
        ys.iter_mut()
            .zip(xs.iter())
            .for_each(|(y, &x)| *y = (self.func)(x, *y));
        Ok(ys)
    }
}

pub struct SubIntegrators {
    partial: Vec<(Range<usize>, Box<dyn Integrator + Sync + Send>)>,
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

    pub fn interpolation_mapped<'a, M>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
        map: M,
    ) -> ApplyFunc<SubInterpolations<'a>, M>
    where
        M: Fn(f64, f64) -> f64 + Sync + Send + 'a,
    {
        assert_eq!(xs.len(), ys.len());
        let mut subinterpolations = SubInterpolations::empty();
        for (range, integrator) in self.partial.iter() {
            if let Some(interpolation) =
                integrator.interpolation(&xs[range.clone()], &ys[range.clone()])
            {
                subinterpolations.partials.push(interpolation);
            } else {
                // println!(
                //     "Got linear interpolator between {} and {}",
                //     xs[range.start],
                //     ys[range.end - 1]
                // );
                subinterpolations
                    .partials
                    .push(Box::new(LinearInterpolation::new(
                        &xs[range.clone()],
                        &ys[range.clone()],
                    )));
            }
        }
        ApplyFunc::new(subinterpolations, map)
    }

    pub fn interpolation<'a>(&self, xs: &'a [f64], ys: &'a [f64]) -> SubInterpolations<'a> {
        assert_eq!(xs.len(), ys.len());
        let mut subinterpolations = SubInterpolations::empty();
        for (range, integrator) in self.partial.iter() {
            if let Some(interpolation) =
                integrator.interpolation(&xs[range.clone()], &ys[range.clone()])
            {
                subinterpolations.partials.push(interpolation);
            } else {
                // println!(
                //     "Got linear interpolator between {} and {}",
                //     xs[range.start],
                //     ys[range.end - 1]
                // );
                subinterpolations
                    .partials
                    .push(Box::new(LinearInterpolation::new(
                        &xs[range.clone()],
                        &ys[range.clone()],
                    )));
            }
        }
        subinterpolations
    }
}

impl Integrator for SubIntegrators {
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(f64, f64) -> f64 + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.partial
            .iter()
            .map(|(range, integrator)| {
                integrator.integrate_mapped(&ys[range.clone()], map, epsilon)
            })
            .sum()
    }

    fn interpolation_mapped<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(f64, f64) -> f64 + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation_mapped(xs, ys, map)))
    }
    fn integrate(&self, ys: &[f64], epsilon: f64) -> f64 {
        self.partial
            .iter()
            .map(|(range, integrator)| integrator.integrate(&ys[range.clone()], epsilon))
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
    fn interpolation<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation(xs, ys)))
    }
}

pub struct LinearInterpolation<'s> {
    x_vec: &'s [f64],
    y_vec: &'s [f64],
}

impl<'s> LinearInterpolation<'s> {
    pub fn new(x_vec: &'s [f64], y_vec: &'s [f64]) -> Self {
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

                    t * self.y_vec[idx] + (1.0 - t) * self.y_vec[idx + 1]
                }
            }
        }
    }

    pub fn eval_many(&self, xs: &[f64], ys_out: &mut [f64]) {
        if self.x_vec.is_empty() {
            ys_out.iter_mut().for_each(|y| *y = 0.0);
            return;
        }
        let mut ptr = 0;
        for (x, y) in xs.iter().zip(ys_out.iter_mut()) {
            while ptr < self.x_vec.len() - 1 && *x > self.x_vec[ptr + 1] {
                ptr += 1;
            }

            if *x < self.x_vec[0] || ptr >= self.x_vec.len() - 1 {
                *y = self.y_vec[ptr];
            } else {
                let t = (*x - self.x_vec[ptr]) / (self.x_vec[ptr + 1] - self.x_vec[ptr]);
                assert!(t >= 0.0);
                assert!(t <= 1.0);
                *y = t * self.y_vec[ptr + 1] + (1.0 - t) * self.y_vec[ptr];
            }
        }
    }
}

impl<'s> Interpolation for LinearInterpolation<'s> {
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.x_vec.first().cloned().unwrap_or(0.0)..=self.x_vec.last().cloned().unwrap_or(0.0)
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        self.eval_many(xs, ys_out);
        Ok(ys_out)
    }
}

pub struct SubInterpolations<'a> {
    pub partials: Vec<Box<dyn Interpolation + 'a>>,
}

impl<'a> SubInterpolations<'a> {
    pub fn empty() -> Self {
        Self {
            partials: Vec::new(),
        }
    }
    pub fn new(partials: Vec<Box<dyn Interpolation + 'a>>) -> Self {
        Self { partials }
    }
}

impl<'s> Interpolation for SubInterpolations<'s> {
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.partials
            .first()
            .map(|partial| partial.interpolate_range().start().clone())
            .unwrap_or(0.0)
            ..=self
                .partials
                .first()
                .map(|partial| partial.interpolate_range().start().clone())
                .unwrap_or(0.0)
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let mut ptr = 0;
        for interpolation in self.partials.iter() {
            let range = interpolation.interpolate_range();

            let end = ptr
                + match xs[ptr..].binary_search_by(|x| x.total_cmp(range.end())) {
                    Ok(idx) | Err(idx) => idx,
                };

            interpolation.interpolate(&xs[ptr..end], &mut ys_out[ptr..end])?;

            ptr = end;
            if ptr == xs.len() {
                break;
            }
        }

        if ptr != xs.len() {
            if let Some(interpolation) = self.partials.last() {
                interpolation.interpolate(&xs[ptr..], &mut ys_out[ptr..])?;
            } else {
                ys_out[ptr..].iter_mut().for_each(|x| *x = 0.0);
            }
        }

        Ok(ys_out)
    }
}
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
        map: &(dyn Fn(f64, f64) -> f64 + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.interpolation_mapped(ys, map).integral(epsilon)
    }
    fn interpolation_mapped<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(f64, f64) -> f64 + Send + Sync + 'a),
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
        M: Fn(f64, f64) -> f64 + Send + Sync,
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
        map: &(dyn Fn(f64, f64) -> f64 + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.interpolation_mapped(ys, map).integral(epsilon)
    }
    fn interpolation_mapped<'a>(
        &self,
        _xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(f64, f64) -> f64 + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        Some(Box::new(self.interpolation_mapped(ys, map)))
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
        M: Fn(f64, f64) -> f64 + Send + Sync,
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

pub struct GaussIntegrator {
    grid: Grid<f64>,
}

impl Integrator for GaussIntegrator {
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(f64, f64) -> f64 + Send + Sync),
        _epsilon: f64,
    ) -> f64 {
        let ys = Vec::from_iter(
            self.grid
                .points
                .iter()
                .cloned()
                .zip(ys.iter().cloned())
                .map(|(x, y)| (map)(x, y)),
        );
        self.grid.eval(&ys)
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

#[cfg(test)]
mod tests {
    use std::f64;

    use la::{BlasLib, LapackeLib};

    use crate::integrate::{
        Integrator, Interpolation, LinearInterpolation, MonotoneCubicIntegrator,
        NaturalCubicIntegrator, SubIntegrators,
    };

    #[test]
    fn subinterpolations() {
        let test_func = f64::cos;
        let xs_vec = Vec::from_iter((0..=100).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let blas_lib = BlasLib::new().unwrap();
        let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();

        let mut subintegrators = SubIntegrators::new();

        subintegrators.push(
            0..51,
            NaturalCubicIntegrator::new(&xs_vec[0..51], lapacke_lib.functions_static()),
        );
        subintegrators.push(50..101, MonotoneCubicIntegrator::new(&xs_vec[50..101]));

        let interpolation = subintegrators.interpolation(&xs_vec, &ys_vec);

        println!("Len: {}", interpolation.partials.len());

        let mut many_result = vec![0.0; test_xs_vec.len()];
        interpolation
            .interpolate(&test_xs_vec, &mut many_result)
            .unwrap();

        for (idx, x) in test_xs_vec.iter().enumerate() {
            let answer = test_func(*x);

            let result = many_result[idx];

            // Test accuracy
            assert!(
                (result - answer).abs() <= 8e-3,
                "Accuracy insufficient, {} vs {}, difference {} at x={}",
                result,
                answer,
                result - answer,
                x
            );
        }
    }

    #[test]
    fn linear_interp() {
        let test_func = f64::cos;
        let xs_vec = Vec::from_iter((0..=100).map(|i| i as f64 / 10.0));
        let test_xs_vec = Vec::from_iter((0..=1000).map(|i| i as f64 / 100.0));
        let ys_vec = xs_vec.iter().cloned().map(test_func).collect::<Vec<_>>();

        let interpolation = LinearInterpolation::new(&xs_vec, &ys_vec);

        // let interpolation = subintegrators.interpolation(&xs_vec, &ys_vec);

        let mut many_result = vec![0.0; test_xs_vec.len()];
        interpolation
            .interpolate(&test_xs_vec, &mut many_result)
            .unwrap();

        for (idx, x) in test_xs_vec.iter().enumerate() {
            let answer = test_func(*x);

            let result = many_result[idx];

            // Test accuracy
            assert!(
                (result - answer).abs() <= 2e-3,
                "Accuracy insufficient, {} vs {}, difference {} at x={}",
                result,
                answer,
                result - answer,
                x
            );
        }
    }

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
