use std::{
    fs::File,
    io::{BufWriter, Write},
};

use la::{BlasLib, LapackeLib};
use nalgebra::DVector;

fn main() {
    // let mut x_vec = Vec::new();
    // let mut y_vec = Vec::new();
    // for line in include_str!("../T=100eV").lines() {
    //     x_vec.push(
    //         -line
    //             .split_whitespace()
    //             .nth(0)
    //             .unwrap()
    //             .parse::<f64>()
    //             .unwrap()
    //             * 1000.0,
    //     );
    //     y_vec.push(
    //         line.split_whitespace()
    //             .nth(1)
    //             .unwrap()
    //             .parse::<f64>()
    //             .unwrap(),
    //     );
    // }

    let test_func = f64::sin;
    let x_vec = Vec::from_iter((0..=100).map(|i| i as f64 / 10.0));
    let y_vec = x_vec.iter().cloned().map(test_func).collect::<Vec<_>>();
    println!("{:?}", x_vec);
    let widths = x_vec
        .windows(2)
        .map(|window| window[1] - window[0])
        .collect::<Vec<_>>();
    println!("{:?}", widths);
    // let y_vec = Vec::from_iter(x_vec.iter().map(|&x| x.sin()));

    let n_func = y_vec.len() - 1;
    println!("n_func: {}", n_func);
    let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
    let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
    let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
    let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };

    let mut d = DVector::<f64>::from_vec(
        (0..n_func - 1)
            .map(|i| 4.0 * (1.0 / (widths[i + 1] * widths[i]) + 1.0 / (widths[i] * widths[i])))
            .collect::<Vec<_>>(),
    );
    let mut du = DVector::<f64>::from_vec(
        (0..n_func - 2)
            .map(|i| 2.0 / (widths[i + 1] * widths[i + 1]))
            .collect::<Vec<_>>(),
    );
    let mut dl = DVector::<f64>::from_vec(
        (1..n_func - 1)
            .map(|i| 2.0 / (widths[i] * widths[i - 1]))
            .collect::<Vec<_>>(),
    );
    let mut b = DVector::<f64>::from_vec(
        (0..n_func - 1)
            .map(|i| {
                6.0 * (y_vec[i + 2] / (widths[i + 1] * widths[i + 1])
                    - y_vec[i] / (widths[i] * widths[i])
                    + y_vec[i + 1]
                        * (1.0 / (widths[i] * widths[i]) - 1.0 / (widths[i + 1] * widths[i + 1])))
            })
            .collect::<Vec<_>>(),
    );

    // let b_copy = b;

    let blas_lib = BlasLib::new().unwrap();
    let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();
    let lapacke = lapacke_lib.functions();

    lapacke.dgtsv(&mut d, &mut du, &mut dl, &mut b).unwrap();

    println!("{}", b);

    let file = File::create("y.dat").unwrap();
    let mut file = BufWriter::new(file);

    for (x, y) in x_vec.iter().zip(y_vec.iter()) {
        writeln!(&mut file, "{} {}", x, y).unwrap();
    }

    let file = File::create("out.dat").unwrap();
    let mut file = BufWriter::new(file);

    for i in 1..3000 as usize {
        let idx = i * n_func / 3000;
        let x = (i * n_func).rem_euclid(3000 as usize) as f64 / 3000.0 * widths[idx];
        let global_x = x_vec[idx] + x;

        let (c0, c1, c2, c3);

        c0 = y_vec[idx];
        c1 = y_vec[idx + 1];

        if idx == 0 {
            c3 = b[0];
            c2 = (-3.0 * c0 + 3.0 * c1 - c3) / 2.0;
            // c2 = 0.0
        } else if idx >= n_func - 1 {
            c2 = b[n_func - 2] * widths[n_func - 1] / widths[n_func - 2];
            c3 = (-3.0 * c0 + 3.0 * c1 - c2) / 2.0;
            // c3 = 0.0;
        } else {
            c2 = b[idx - 1] * widths[idx] / widths[idx - 1];
            c3 = b[idx];
        }

        if i * n_func % 3000 <= n_func {
            println!(
                "{} Curvature: {}",
                idx,
                (-6.0 * c0 + 6.0 * c1 - 4.0 * c2 - 2.0 * c3) / (widths[idx] * widths[idx])
            );
        }

        let scaled_x = x / widths[idx];

        // println!("Scaled x: {}", scaled_x);

        let q = c0 * h00(scaled_x) + c1 * h01(scaled_x) + c2 * h10(scaled_x) + c3 * h11(scaled_x);

        let answer = test_func(global_x);

        writeln!(&mut file, "{} {} {}", global_x, q, answer).unwrap();
    }

    println!("Hello, world!");
}

// fn main() {
//     let x_vec = Vec::from_iter((0..=40).map(|i| i as f64));
//     let y_vec = Vec::from_iter(x_vec.iter().map(|&x| x.ln() / x));

//     let n_func = y_vec.len() - 1;
//     let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
//     let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
//     let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
//     let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };

//     let mut d = DVector::<f64>::from_vec(vec![4.0; n_func - 1]);
//     let mut du = DVector::<f64>::from_vec(vec![1.0; n_func - 1]);
//     let mut dl = DVector::<f64>::from_vec(vec![1.0; n_func - 1]);
//     let mut b = DVector::<f64>::zeros(n_func - 1);

//     b.iter_mut().enumerate().for_each(|(idx, b)| {
//         *b = (y_vec[idx + 2] - y_vec[idx]) * 3.0;
//     });

//     let blas_lib = BlasLib::new().unwrap();
//     let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();
//     let lapacke = lapacke_lib.functions();

//     lapacke.dgtsv(&mut d, &mut du, &mut dl, &mut b);

//     println!("{}", b);

//     let file = File::create("y.dat").unwrap();
//     let mut file = BufWriter::new(file);

//     for (i, y) in y_vec.iter().enumerate() {
//         writeln!(&mut file, "{} {}", i, y).unwrap();
//     }

//     let file = File::create("out.dat").unwrap();
//     let mut file = BufWriter::new(file);

//     for i in 1..1000 as usize {
//         let idx = i * n_func / 1000;
//         let x = (i * n_func).rem_euclid(1000 as usize) as f64 / 1000.0;
//         let global_x = (i * n_func) as f64 / 1000.0;

//         let (c0, c1, c2, c3);

//         c0 = y_vec[idx];
//         c1 = y_vec[idx + 1];

//         if idx == 0 {
//             c3 = b[0];
//             c2 = (-3.0 * c0 + 3.0 * c1 - c3) / 2.0;
//         } else if idx == n_func - 1 {
//             c2 = b[n_func - 2];
//             c3 = (3.0 * c0 - 3.0 * c1 + c2) / 2.0;
//         } else {
//             c2 = b[idx - 1];
//             c3 = b[idx];
//         }

//         if i * n_func % 1000 <= n_func {
//             println!("Curvature: {}", -6.0 * c0 + 6.0 * c1 - 4.0 * c2 - 2.0 * c3);
//         }

//         let q = c0 * h00(x) + c1 * h01(x) + c2 * h10(x) + c3 * h11(x);

//         writeln!(&mut file, "{} {}", global_x, q).unwrap();
//     }

//     println!("Hello, world!");
// }
