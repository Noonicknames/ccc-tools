use std::ops::Range;

use crate::integrators::interpolation::{
    ApplyFunc, Interpolation, LinearInterpolation, SubInterpolations,
};

mod gauss;
pub mod interpolation;
mod monotone_cubic;
mod natural_cubic;

pub use self::{gauss::*, monotone_cubic::*, natural_cubic::*};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegrationKind {
    #[default]
    MonotoneCubic,
    NaturalCubic,
    AutoGauss,
    Gauss,
}

pub trait Integrator: Send + Sync {
    fn integrate(&self, ys: &[f64], epsilon: f64) -> f64;
    fn integrate_interest_points(&self, ys: &[f64], interest_points: &[f64], epsilon: f64) -> f64;

    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        epsilon: f64,
    ) -> f64;
    fn integrate_len(&self) -> usize;

    fn interpolation_mapped<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(&[f64], &mut [f64]) + Send + Sync + 'a),
    ) -> Option<Box<dyn Interpolation + 'a>> {
        assert_eq!(xs.len(), ys.len());
        _ = (xs, ys, map);
        None
    }
    fn integrate_mapped_interest_points(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        interest_points: &[f64],
        epsilon: f64,
    ) -> f64;

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
        M: Fn(&[f64], &mut [f64]) + Sync + Send + 'a,
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
    fn integrate_interest_points(&self, ys: &[f64], interest_points: &[f64], epsilon: f64) -> f64 {
        self.partial
            .iter()
            .map(|(range, integrator)| {
                integrator.integrate_interest_points(&ys[range.clone()], interest_points, epsilon)
            })
            .sum()
    }
    fn integrate_mapped(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        epsilon: f64,
    ) -> f64 {
        self.partial
            .iter()
            .map(|(range, integrator)| {
                integrator.integrate_mapped(&ys[range.clone()], map, epsilon)
            })
            .sum()
    }

    fn integrate_mapped_interest_points(
        &self,
        ys: &[f64],
        map: &(dyn Fn(&[f64], &mut [f64]) + Send + Sync),
        interest_points: &[f64],
        epsilon: f64,
    ) -> f64 {
        self.partial
            .iter()
            .map(|(range, integrator)| {
                integrator.integrate_mapped_interest_points(&ys[range.clone()], map, interest_points, epsilon)
            })
            .sum()
    }

    fn interpolation_mapped<'a>(
        &self,
        xs: &'a [f64],
        ys: &'a [f64],
        map: &'a (dyn Fn(&[f64], &mut [f64]) + Send + Sync + 'a),
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
