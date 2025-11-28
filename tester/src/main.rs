use std::{fs::File, io::Write};

use la::{BlasLib, LapackeLib};
use nalgebra::DVector;

fn main() {
    let y_vec = vec![1.0, 1.0 / 2.0, 1.0 / 4.0, 1.0 / 8.0];
    let n = y_vec.len() - 1;
    let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
    let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
    let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
    let h11 = |t: f64| -> f64 { t * t * (1.0 - t) };

    let mut d = DVector::from_vec(vec![1.0; n]);
    let mut du = DVector::from_vec(vec![1.0; n]);
    let mut dl = DVector::from_vec(vec![1.0; n]);
    let mut b = DVector::from_vec(vec![1.0; n]);

    let blas_lib = BlasLib::new().unwrap();
    let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();
    let lapacke = lapacke_lib.functions();

    lapacke.dgtsv(&mut d, &mut du, &mut dl, &mut b);

    println!("{}", b);

    let mut file = File::create("out.dat").unwrap();

    for i in 1..1000 as usize {
        let idx = i * n / 1000;
        let x = (i * n).rem_euclid(1000 as usize) as f64 / 1000.0;
        let global_x = (i * n) as f64 / 1000.0;
        let (c0, c1, c2, c3) = (y_vec[idx], y_vec[idx + 1], b);
        let q = c0 * h00(x) + c1 * h01(x);

        writeln!(&mut file, "{} {}", global_x, q).unwrap();
    }

    println!("Hello, world!");
}
