use std::future::Future;

use tokio::io::AsyncBufReadExt;

pub mod sync;

#[cfg(feature = "tokio")]
pub mod r#async;

pub trait AsyncSectionParser {
    type Output;
    fn parse_section(
        file: &mut tokio::io::BufReader<tokio::fs::File>,
        output: &mut Self::Output,
    ) -> impl Future<Output = ()>;
}

pub struct Results {
    units: String,
}

pub struct UnitsParser;

impl AsyncSectionParser for UnitsParser {
    type Output = Results;
    fn parse_section(
        file: &mut tokio::io::BufReader<tokio::fs::File>,
        output: &mut Self::Output,
    ) -> impl Future<Output = ()> {
        async move {
            // Let's read the line
            let mut line = String::new();
            file.read_line(&mut line).await.unwrap(); // Uh oh! No way to handle if it errors lets .unwrap() and crash the whole program!

            // Lets say we *hope* the file looks something like this...
            // Units: m^2
            let units = line.split_whitespace().nth(1).unwrap(); // Get the second word 'm^2'.
            // Again we crash the whole program if the file is not what we expect.
            
            // Check if these are actually units
            if !is_valid_units(units) {
                // Panic means crash.
                panic!("Who designed this, I have no way of handling this error.");
            }

            output.units = units.to_owned();
        }
    }
}

fn is_valid_units(units: &str) -> bool {
    todo!()
}
