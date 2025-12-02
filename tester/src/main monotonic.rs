use std::{
    fs::File,
    io::{BufWriter, Write},
};

fn main() {
    let mut x_vec = Vec::new();
    let mut y_vec = Vec::new();
    for line in include_str!("../T=100eV").lines() {
        let (x, y) = (
            line.split_whitespace()
                .nth(0)
                .unwrap()
                .parse::<f64>()
                .unwrap(),
            line.split_whitespace()
                .nth(1)
                .unwrap()
                .parse::<f64>()
                .unwrap(),
        );

        if Some(x) != x_vec.last().cloned() {
            x_vec.push(x);
            y_vec.push(y);
        } else {
            println!("Duplicate x found, discarding.");
        }
    }

    let delta_vec = x_vec
        .windows(2)
        .zip(y_vec.windows(2))
        .map(|(x_window, y_window)| {
            let &[x0, x1] = x_window else { unreachable!() };
            let &[y0, y1] = y_window else { unreachable!() };
            (y1 - y0) / (x1 - x0)
        })
        .collect::<Vec<_>>();

    println!("{:?}", delta_vec);

    let mut m_vec = delta_vec
        .first()
        .cloned()
        .into_iter()
        .chain(delta_vec.windows(2).map(|window| {
            let &[l, r] = window else { unreachable!() };
            if l * r <= f64::EPSILON.sqrt() {
                0.0
            } else {
                (l + r) / 2.0
            }
        }))
        .chain(delta_vec.last().cloned())
        .collect::<Vec<_>>();

    m_vec.last_mut().iter_mut().for_each(|last| **last = 0.0);

    println!("{:?}", m_vec);

    assert_eq!(m_vec.len(), delta_vec.len() + 1);

    for i in 0..delta_vec.len() {
        if delta_vec[i] == 0.0 {
            m_vec[i] = 0.0;
            m_vec[i + 1] = 0.0;
            continue;
        } else {
            let alpha = m_vec[i] / delta_vec[i];
            let beta = if i > 0 {
                m_vec[i] / delta_vec[i - 1]
            } else {
                0.0
            };

            if alpha < 0.0 || beta < 0.0 {
                m_vec[i] = 0.0;
            }

            let norm_sq = alpha * alpha + beta * beta;
            if norm_sq > 9.0 {
                let tau = 3.0 / norm_sq.sqrt();
                m_vec[i] = tau * alpha * delta_vec[i];
                m_vec[i + 1] = tau * beta * delta_vec[i];
            }
        }
    }

    let h00 = |t: f64| -> f64 { (1.0 + 2.0 * t) * (1.0 - t) * (1.0 - t) };
    let h01 = |t: f64| -> f64 { t * t * (3.0 - 2.0 * t) };
    let h10 = |t: f64| -> f64 { t * (1.0 - t) * (1.0 - t) };
    let h11 = |t: f64| -> f64 { t * t * (t - 1.0) };

    let file = File::create("out.dat").unwrap();
    let mut file = BufWriter::new(file);

    for (k, x_window) in x_vec.windows(2).enumerate() {
        let &[left, right] = x_window else {
            unreachable!()
        };
        let width = right - left;

        for i in 0..100 {
            let t = i as f64 / 100.0;
            let global_x = t * width + left;

            let y = y_vec[k] * h00(t)
                + y_vec[k + 1] * h01(t)
                + width * m_vec[k] * h10(t)
                + width * m_vec[k + 1] * h11(t);
            // let y = y.exp();
            // let y = y * global_x / (global_x * 27.2114 / 100.0).exp();
            writeln!(&mut file, "{} {}", global_x, y).unwrap();
        }
    }

    let mut file = BufWriter::new(File::create("y.dat").unwrap());

    for (x, y) in x_vec.iter().zip(y_vec.iter()) {
        writeln!(&mut file, "{} {}", x, y).unwrap();
    }
}
