use std::ops::RangeInclusive;

use crate::nodetree::NodeTree;

pub trait Interpolation: Send + Sync {
    fn interpolate_range(&self) -> RangeInclusive<f64>;
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()>;
    fn integral_interest_points(&self, interest_points: &[f64], epsilon: f64) -> f64 {
        #[derive(Default, Debug)]
        struct NodeData {
            range: (f64, f64),
            contained_points: (usize, usize),
        }

        impl NodeData {
            pub fn contained_points_len(&self) -> usize {
                self.contained_points.1 - self.contained_points.0
            }
            pub fn split(&self, interest_points: &[f64]) -> (NodeData, NodeData) {
                let split_point = (self.range.0 + self.range.1) / 2.0;
                let split_idx = &interest_points[self.contained_points.0..self.contained_points.1]
                    .iter()
                    .position(|val| *val >= split_point)
                    .unwrap_or(self.contained_points_len() - 1)
                    + self.contained_points.0;

                let child_1 = NodeData {
                    range: (self.range.0, split_point),
                    contained_points: (self.contained_points.0, split_idx),
                };
                let child_2 = NodeData {
                    range: (split_point, self.range.1),
                    contained_points: (split_idx, self.contained_points.1),
                };

                (child_1, child_2)
            }
        }

        let interp_range = self.interpolate_range();

        let mut tree: NodeTree<NodeData> = NodeTree::new(NodeData {
            range: (*interp_range.start(), *interp_range.end()),
            contained_points: (
                interest_points
                    .iter()
                    .position(|val| val >= interp_range.start())
                    .unwrap_or(interest_points.len() - 1),
                interest_points
                    .iter()
                    .position(|val| val > interp_range.end())
                    .unwrap_or(interest_points.len() - 1),
            ),
        });

        let mut node = tree.root();

        'outer: loop {
            let value = node.value(&tree);
            assert!(!node.has_children(&tree));
            if value.contained_points_len() > 1 {
                let (l_child, r_child) = value.split(interest_points);
                node.create_children(&mut tree, l_child, r_child);
                node = node.left_child(&tree).unwrap();
            } else {
                // Get next leaf
                loop {
                    let Some(parent) = node.parent(&tree) else {
                        break 'outer;
                    };
                    let right_child = parent.right_child(&tree).unwrap();
                    if node == right_child {
                        node = parent;
                    } else {
                        node = right_child;
                        break;
                    }
                }
            }
        }

        let mut iter = tree.leaf_depth_iter();

        let mut prev_depth = iter.next().unwrap().1;
        let mut prev_end = *interp_range.start();

        let mut sum = 0.0;
        let mut c = 0.0;

        let interp_width = *interp_range.end() - *interp_range.start();

        while let Some(next) = iter.find(|(_, depth)| *depth != prev_depth) {
            let next_end = next.0.value(&tree).range.1;

            let num = SubInterpolation::new(self, prev_end..=next_end)
                .integral(epsilon * (next_end - prev_end) / interp_width);

            let y = num - c;
            let t = sum + y;
            c = (t - sum) - y;

            sum = t;

            prev_end = next_end;
            prev_depth = next.1;
        }

        if prev_end != *interp_range.end() {
            let num = SubInterpolation::new(self, prev_end..=*interp_range.end())
                .integral(epsilon * (interp_range.end() - prev_end) / interp_width);

            let y = num - c;
            let t = sum + y;
            c = (t - sum) - y;

            sum = t;
        }

        sum
    }
    fn integral(&self, epsilon: f64) -> f64 {
        let interpolate_range = self.interpolate_range();

        let mut intervals = 4;

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

            result += 32.0 * kahan_sum(ys[1..=intervals - 1].iter().step_by(2).cloned());
            result += 12.0 * kahan_sum(ys[2..=intervals - 2].iter().step_by(4).cloned());
            if intervals >= 8 {
                result += 14.0 * kahan_sum(ys[4..=intervals - 4].iter().step_by(4).cloned());
            }

            result = result * step * 2.0 / 45.0;

            result
        };

        loop {
            let new_xs = Vec::from_iter((0..intervals).map(|i| {
                let t = (2 * i + 1) as f64 / (2 * intervals) as f64;
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

                result += 32.0 * kahan_sum(ys[1..=intervals - 1].iter().step_by(2).cloned());
                result += 12.0 * kahan_sum(ys[2..=intervals - 2].iter().step_by(4).cloned());
                result += 14.0 * kahan_sum(ys[4..=intervals - 4].iter().step_by(4).cloned());

                result = result * step * 2.0 / 45.0;

                result
            };

            println!(
                "Error {}: {}",
                intervals,
                (current_answer - prev_answer).abs()
            );
            if (current_answer - prev_answer).abs() < epsilon {
                println!("Partitions: {}", intervals);
                break current_answer;
            } else {
                prev_answer = current_answer;
            }
        }
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

impl<T> Interpolation for &T
where
    T: Interpolation + ?Sized,
{
    fn integral(&self, epsilon: f64) -> f64 {
        T::integral(&self, epsilon)
    }
    fn integral_interest_points(&self, interest_points: &[f64], epsilon: f64) -> f64 {
        T::integral_interest_points(&self, interest_points, epsilon)
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        T::interpolate(&self, xs, ys_out)
    }
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        T::interpolate_range(&self)
    }
}

pub struct SubInterpolation<I>
where
    I: Interpolation,
{
    interpolation: I,
    range: RangeInclusive<f64>,
}

impl<I> SubInterpolation<I>
where
    I: Interpolation,
{
    pub fn new(interpolation: I, range: RangeInclusive<f64>) -> Self {
        Self {
            interpolation,
            range,
        }
    }
}

impl<I> Interpolation for SubInterpolation<I>
where
    I: Interpolation,
{
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let new_start = xs
            .iter()
            .position(|x| *x >= *self.range.start())
            .unwrap_or(xs.len());
        let new_end = xs[new_start..]
            .iter()
            .position(|x| *x > *self.range.end())
            .unwrap_or(xs.len() - new_start)
            + new_start;

        if new_start != new_end {
            self.interpolation
                .interpolate(&xs[new_start..new_end], &mut ys_out[new_start..new_end])?;
        }

        ys_out[..new_start].iter_mut().for_each(|y| *y = 0.0);
        ys_out[new_end..].iter_mut().for_each(|y| *y = 0.0);

        Ok(ys_out)
    }
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.range.clone()
    }
}

pub struct ApplyFunc<I, F>
where
    F: Fn(&[f64], &mut [f64]) + Sync + Send,
{
    inner: I,
    func: F,
}

impl<I, F> ApplyFunc<I, F>
where
    F: Fn(&[f64], &mut [f64]) + Sync + Send,
{
    pub fn new(inner: I, func: F) -> Self {
        Self { inner, func }
    }
}

impl<I, F> Interpolation for ApplyFunc<I, F>
where
    I: Interpolation,
    F: Fn(&[f64], &mut [f64]) + Sync + Send,
{
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.inner.interpolate_range()
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let ys = self.inner.interpolate(xs, ys_out)?;

        (self.func)(xs, ys);

        Ok(ys)
    }
}

impl<F> Interpolation for ApplyFunc<Box<dyn Interpolation>, F>
where
    F: Fn(&[f64], &mut [f64]) + Sync + Send,
{
    fn interpolate_range(&self) -> RangeInclusive<f64> {
        self.inner.interpolate_range()
    }
    fn interpolate<'a>(&self, xs: &[f64], ys_out: &'a mut [f64]) -> Result<&'a mut [f64], ()> {
        let ys = self.inner.interpolate(xs, ys_out)?;
        (self.func)(xs, ys);
        Ok(ys)
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

#[cfg(test)]
mod tests {
    use crate::integrators::{
        MonotoneCubicIntegrator,
        interpolation::{ApplyFunc, Interpolation},
    };

    #[test]
    fn integrate_interest_points() {
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
                std::f64::consts::PI / 4.0,
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
            let interpolation = integrator.interpolation(&ys_vec);
            let result = interpolation.integral_interest_points(&xs_vec, 1e-6);
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
