use la::{EigenRange, LapackeFunctions};
use nalgebra::{ComplexField, Const, DMatrix, DVector, DVectorView, Dyn, MatrixView};
use rustnomial::{Evaluable, Integrable, Polynomial, Roots, polynomial};
use serde::{Deserialize, Serialize};
use special::Gamma;

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq)]
pub struct Grid<T>
where
    T: ComplexField,
{
    pub points: DMatrix<T>,
    pub weights: DVector<T>,
}

pub type GridPointRef<'a, T> = MatrixView<'a, T, Const<1>, Dyn, Const<1>, Dyn>;

impl<T> Grid<T>
where
    T: ComplexField,
{
    pub fn dimensions(&self) -> usize {
        self.points.shape().1
    }
    /// Translate the points used for the rule.
    ///
    /// # Errors
    /// This will only error if the provided vector does not match the dimensions of the rule.
    pub fn translate(&mut self, x: &DVector<T>) -> Result<(), ()> {
        if self.dimensions() != x.len() {
            return Err(());
        }
        self.points.row_iter_mut().for_each(|mut row| row += x);
        Ok(())
    }

    /// Scale the i.
    ///
    /// For example, scaling by 2.0 will change bounds from `[-1,1]` to `[-2,2]`.
    ///
    /// # Errors
    /// This will only error if the provided vector does not match the dimensions of the rule.
    pub fn scale_weight(&mut self, scale: T) -> Result<(), ()> {
        self.points
            .row_iter_mut()
            .for_each(|mut row| row *= scale.clone());
        self.weights *= scale;
        Ok(())
    }

    /// Scale the integration bounds and adjust the weights accordingly.
    ///
    /// For example, scaling by 2.0 will change bounds from `[-1,1]` to `[-2,2]`.
    pub fn scale_x(&mut self, scale: T) {
        self.points
            .row_iter_mut()
            .for_each(|mut row| row *= scale.clone());
        self.weights *= scale;
    }

    pub fn eval_fn(&self, f: impl FnMut(GridPointRef<T>) -> T) -> T {
        let f_points = self.points.row_iter().map(f).collect::<Vec<_>>();
        self.eval(&f_points)
    }
    pub fn eval(&self, f_points: &[T]) -> T {
        let f_points = DVectorView::from_slice(f_points, f_points.len());
        let weights = DVectorView::from_slice(self.weights.as_slice(), self.weights.len());
        weights.dot(&f_points)
    }
    // // Disabled because nalgebra::stack introduced in nalgebra@0.33, MSRV is higher than 1.85
    // pub fn multi_combine(&self, other_grid: &Grid<T>) -> Grid<T> {
    //     let points = &self.points;
    //     let other_points = &other_grid.points;
    //     let weights = &self.weights;
    //     let other_weights = &other_grid.weights;

    //     let ones_vec_self = nalgebra::DVector::from_vec(vec![T::one(); points.shape().0]);
    //     let ones_vec_other = nalgebra::DVector::from_vec(vec![T::one(); other_points.shape().0]);

    //     let multi_points = nalgebra::stack![
    //         ones_vec_other.kronecker(&points),
    //         other_points.kronecker(&ones_vec_self)
    //     ];

    //     let multi_weights = other_weights.kronecker(&weights);

    //     let shape = multi_points.shape();
    //     Grid {
    //         weights: multi_weights,
    //         points: multi_points.reshape_generic(Dyn(shape.0), Dyn(shape.1)),
    //     }
    // }
}

pub fn legendre_grid_roots(lapacke: &LapackeFunctions, n: usize) -> Grid<f64> {
    golub_welsch(lapacke, legendre_ttrr(), 2.0, n)
}

pub fn legendre_grid_fixed_x(x: &[f64]) -> Grid<f64> {
    let n = x.len();
    let legendre_cache = Vec::from_iter((0..n).map(|i| legendre(i)));

    let y_vec = DVector::from_iterator(
        n,
        (0..n).map(|i| legendre_cache[i].integral().eval(-1.0, 1.0) as f64),
    );

    let x_vec = DVector::from_vec(x.to_vec());

    let jacobian = DMatrix::from_iterator(
        n,
        n,
        x_vec
            .iter()
            .flat_map(|x| (0..n).map(|i| (i, *x)))
            .map(|(i, x)| legendre_cache[i].eval(x as f64) as f64),
    );

    let theta = jacobian.try_inverse().unwrap() * y_vec;

    let shape = x_vec.shape();
    Grid {
        points: x_vec.reshape_generic(Dyn(shape.0), Dyn(shape.1)),
        weights: theta,
    }
}

/// Describes a three term recurrence relation.
///
/// P_{n+1} = (a_{n}x + b_{n})P_{n} + c_{n}P_{n-1}
pub struct TTRR<FA, FB, FC>
where
    FA: FnMut(usize) -> f64,
    FB: FnMut(usize) -> f64,
    FC: FnMut(usize) -> f64,
{
    pub a: FA,
    pub b: FB,
    pub c: FC,
}

impl<FA, FB, FC> TTRR<FA, FB, FC>
where
    FA: FnMut(usize) -> f64,
    FB: FnMut(usize) -> f64,
    FC: FnMut(usize) -> f64,
{
    pub fn poly(
        &mut self,
        f_0: Polynomial<f64>,
        f_1: Polynomial<f64>,
        n: usize,
    ) -> Polynomial<f64> {
        match n {
            0 => f_0,
            1 => f_1,
            n => {
                let x = polynomial![1.0, 0.0];

                let mut poly_prev = f_0;
                let mut poly = f_1;

                for n in 1..n {
                    let temp = poly.clone();
                    poly = x.clone() * poly.clone() * (self.a)(n)
                        + poly * (self.b)(n)
                        + poly_prev * (self.c)(n);
                    poly_prev = temp;
                }
                poly
            }
        }
    }
}

pub fn legendre_ttrr()
-> TTRR<impl FnMut(usize) -> f64, impl FnMut(usize) -> f64, impl FnMut(usize) -> f64> {
    TTRR {
        a: |k| (2 * k + 1) as f64 / (k + 1) as f64,
        b: |_| 0.0,
        c: |k| -(k as f64) / (k + 1) as f64,
    }
}

pub fn laguerre_ttrr()
-> TTRR<impl FnMut(usize) -> f64, impl FnMut(usize) -> f64, impl FnMut(usize) -> f64> {
    TTRR {
        a: |k| -1.0 / (k + 1) as f64,
        b: |k| (2 * k + 1) as f64 / (k + 1) as f64,
        c: |k| -(k as f64) / (k + 1) as f64,
    }
}

pub fn moment_fitted(nodes: &[f64], moment: impl FnMut(usize) -> f64) -> Grid<f64> {
    let n = nodes.len();
    let moments_cache = (0..=n * 2 - 2).map(moment).collect::<Vec<f64>>();

    let inner_prod = |l: &[f64], r: &[f64]| -> f64 {
        debug_assert!(l.len() == r.len());

        l.iter()
            .enumerate()
            .map(|(nl, l)| {
                r.iter()
                    .enumerate()
                    .map(|(nr, r)| l * r * moments_cache[nl + nr])
                    .sum::<f64>()
            })
            .sum::<f64>()
    };

    // Find an orthonormal basis under the inner product above.
    // Gram-Schmidt on ordinary basis [1, 0, 0, ...], [0, 1, 0, ...], [0, 0, 1, ...].
    let mut basis = Vec::from_iter((0..n).map(|i| {
        let mut vec = DVector::zeros(n);
        vec[i] = 1.0;
        vec
    }));

    basis[0] = &basis[0] / inner_prod(basis[0].as_slice(), basis[0].as_slice()).sqrt();

    for i in 1..nodes.len() {
        for j in 0..i {
            basis[i] = &basis[i] - inner_prod(basis[i].as_slice(), basis[j].as_slice()) * &basis[j];
        }
        basis[i] = &basis[i] / inner_prod(basis[i].as_slice(), basis[i].as_slice()).sqrt();
    }

    // Debugging
    // basis.iter().enumerate().for_each(|(i, vec)| {
    //     println!("{}", vec);

    //     for j in 0..i {
    //         println!(
    //             "<{}, {}> = {}",
    //             i,
    //             j,
    //             inner_prod(basis[i].as_slice(), basis[j].as_slice())
    //         );
    //     }
    // });

    let mut p_mat = DMatrix::<f64>::zeros(n, n);

    for (i, &x) in nodes.iter().enumerate() {
        for (j, poly) in basis.iter().enumerate() {
            p_mat[(j, i)] = poly.iter().rev().fold(0.0, |acc, coeff| {
                // Horner evaluation
                acc * x + coeff
            });
        }
    }

    let mut b_mat = DVector::<f64>::zeros(n);

    for (i, poly) in basis.iter().enumerate() {
        b_mat[i] = poly
            .iter()
            .enumerate()
            .map(|(j, coeff)| coeff * moments_cache[j])
            .sum::<f64>();
    }
    
    let svd = p_mat.svd(true, true);

    let w = match svd.solve(&b_mat, std::f64::EPSILON) {
        Ok(w) => w,
        Err(_) => svd
            .recompose()
            .unwrap()
            .lu()
            .solve(&b_mat)
            .expect("LU solve failed"),
    };

    Grid {
        points: DMatrix::from_iterator(n, 1, nodes.iter().cloned()),
        weights: w,
    }
}

/// Perform Golub-Welsch for a specified three term recurrence relation, returning a quadrature rule.
///
/// # What is mu
/// `mu` refers to the integral of the weight function.
pub fn golub_welsch(
    lapacke: &LapackeFunctions,
    mut ttrr: TTRR<impl FnMut(usize) -> f64, impl FnMut(usize) -> f64, impl FnMut(usize) -> f64>,
    mu: f64,
    n: usize,
) -> Grid<f64> {
    let mut d_vec = DVector::from_iterator(n, (0..n).map(|k| -(ttrr.b)(k) / (ttrr.a)(k)));
    let mut e_vec = DVector::from_iterator(
        n,
        (0..n - 1)
            .map(|k| (-(ttrr.c)(k + 1) / ((ttrr.a)(k + 1) * (ttrr.a)(k))).sqrt())
            .chain(Some(0.0)),
    );

    let mut isuppz = DVector::zeros(2 * usize::max(1, n));
    let mut eigs = DVector::<f64>::zeros(n);
    let mut z = DMatrix::zeros(n, n);

    lapacke.dstemr(
        EigenRange::All,
        &mut d_vec,
        &mut e_vec,
        &mut eigs,
        &mut isuppz,
        Some(&mut z),
    );

    // Page 143 Numerical Methods For Special Functions by Amparo Gill to calculate weights.
    let weights = z.row(0).transpose().apply_into(|x| *x = (*x) * (*x)) * mu;

    let shape = eigs.shape();
    Grid {
        points: eigs.reshape_generic(Dyn(shape.0), Dyn(shape.1)),
        weights: weights,
    }
}

/// Returns a grid used to integrate from bounds `[0,+infinity]` with weight function `e^{-x}`
///
/// Utilises Golub-Welsch
pub fn laguerre_grid_roots(n: usize, lapacke: &LapackeFunctions) -> Grid<f64> {
    golub_welsch(lapacke, laguerre_ttrr(), 1.0, n)
}

/// Returns a grid used to integrate from bounds `[0,+infinity]` with weight function `e^{-x}`
///
/// Manually choosing `x` points may not yield a good rule, opt for [laguerre_grid_roots] instead.
pub fn laguerre_grid_fixed_x(x: &[f64]) -> Grid<f64> {
    let n = x.len();
    let laguerre_cache = Vec::from_iter((0..n).map(|i| laguerre(i)));

    let y_vec = DVector::from_iterator(n, (0..n).map(|i| if i == 0 { 1.0 } else { 0.0 }));

    let x_vec = DVector::from_vec(x.to_vec());

    let jacobian = DMatrix::from_iterator(
        n,
        n,
        x_vec
            .iter()
            .flat_map(|x| (0..n).map(|i| (i, *x)))
            .map(|(i, x)| laguerre_cache[i].eval(x as f64) as f64),
    );

    let theta = jacobian.try_inverse().unwrap() * y_vec;

    let shape = x_vec.shape();
    Grid {
        points: x_vec.reshape_generic(Dyn(shape.0), Dyn(shape.1)),
        weights: theta,
    }
}

/// Returns a grid used to integrate from bounds `[0,+infinity]` with weight function `x^{alpha} * e^{-x}`
pub fn gen_laguerre_grid_roots(n: usize, alpha: f64) -> Grid<f64> {
    let laguerre_poly = gen_laguerre(n, alpha);
    let roots = laguerre_poly.roots();
    let roots = match roots {
        Roots::ManyRealRoots(roots) => roots,
        roots => panic!("Expected ManyRealRoots, got {:?}", roots),
    };

    gen_laguerre_grid_fixed_x(&roots, alpha)
}

/// Returns a grid used to integrate from bounds `[0,+infinity]` with weight function `x^{alpha} * e^{-x}`
///
/// Manually choosing `x` points may not yield a good rule, opt for [gen_laguerre_grid_roots] instead.
pub fn gen_laguerre_grid_fixed_x(x: &[f64], alpha: f64) -> Grid<f64> {
    let n = x.len();
    let laguerre_cache = Vec::from_iter((0..n).map(|i| gen_laguerre(i, alpha)));

    // First equation in the Orthogonality section with n = 0.
    // https://en.wikipedia.org/wiki/Laguerre_polynomials#Recurrence_relations
    let y_vec = DVector::from_iterator(
        n,
        (0..n).map(|i| {
            if i == 0 {
                Gamma::gamma(1.0 + alpha)
            } else {
                0.0
            }
        }),
    );

    let x_vec = DVector::from_vec(x.to_vec());

    let jacobian = DMatrix::from_iterator(
        n,
        n,
        x_vec
            .iter()
            .flat_map(|x| (0..n).map(|i| (i, *x)))
            .map(|(i, x)| laguerre_cache[i].eval(x as f64) as f64),
    );

    let theta = jacobian.try_inverse().unwrap() * y_vec;

    let shape = x_vec.shape();
    Grid {
        points: x_vec.reshape_generic(Dyn(shape.0), Dyn(shape.1)),
        weights: theta,
    }
}

pub fn gen_laguerre(n: usize, alpha: f64) -> Polynomial<f64> {
    match n {
        0 => polynomial![1.0],               // 1
        1 => polynomial![-1.0, 1.0 + alpha], // -x + 1 + alpha
        n => {
            let one = polynomial![1.0];
            let x = polynomial![1.0, 0.0];
            let mut poly_prev = gen_laguerre(0, alpha);
            let mut poly = gen_laguerre(1, alpha);

            for n in 1..n {
                let temp = poly.clone();
                poly = (poly * (one.clone() * ((2 * n + 1) as f64 + alpha) - x.clone())
                    - poly_prev * (n as f64 + alpha))
                    / (n + 1) as f64;
                poly_prev = temp;
            }
            poly
        }
    }
}

pub fn laguerre(n: usize) -> Polynomial<f64> {
    match n {
        0 => polynomial![1.0],       // 1
        1 => polynomial![-1.0, 1.0], // 1 - x
        n => {
            let one = polynomial![1.0];
            let x = polynomial![1.0, 0.0];
            let mut poly_prev = laguerre(0);
            let mut poly = laguerre(1);

            for n in 1..n {
                let temp = poly.clone();
                poly = (poly * (one.clone() * (2 * n + 1) as f64 - x.clone())
                    - poly_prev * n as f64)
                    / (n + 1) as f64;
                poly_prev = temp;
            }
            poly
        }
    }
}

pub fn legendre(n: usize) -> Polynomial<f64> {
    match n {
        0 => polynomial![1.0],      // 1
        1 => polynomial![1.0, 0.0], // x
        n => {
            let x = polynomial![1.0, 0.0];
            let mut poly_prev = legendre(0);
            let mut poly = legendre(1);

            for n in 1..n {
                let temp = poly.clone();
                poly = (poly * &x * (2 * n + 1) as f64 - poly_prev * n as f64) / (n + 1) as f64;
                poly_prev = temp;
            }
            poly
        }
    }
}

#[test]
fn test_legendre() {
    use rustnomial::Evaluable;
    let zeroth = legendre(0);
    assert_eq!(zeroth.eval(0.0), 1.0);

    let first = legendre(1);

    assert_eq!(first.eval(-1.0), -1.0);

    for i in 0..64 {
        assert!((legendre(i).eval(1.0) - 1.0).abs() < 1e-8, "Failed {}", i);
    }
}

#[test]
fn test_laguerre() {
    use rustnomial::Evaluable;
    for i in 0..64 {
        assert!((laguerre(i).eval(0.0) - 1.0).abs() < 1e-8, "Failed {}", i);
    }
}
