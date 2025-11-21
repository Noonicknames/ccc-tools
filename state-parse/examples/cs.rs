use std::num::ParseFloatError;

use state_parse::sync::{ParseSource, Parser, SectionParser, make_parse_source};

#[derive(Debug)]
pub enum Units {
    Atomic,
    CentimetreSquared,
    MetreSquared,
}

impl Units {
    pub fn from_str(str: &str) -> Option<Self> {
        Some(match str {
            "a0^2" => Self::Atomic,
            "cm^2" => Self::CentimetreSquared,
            "m^2" => Self::MetreSquared,
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum MyError {
    ParseFloat(ParseFloatError),
    UnexpectedEndOfLine,
    Io(std::io::Error),
}

impl From<ParseFloatError> for MyError {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloat(value)
    }
}

impl From<std::io::Error> for MyError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

#[derive(Debug)]
pub struct ParseOutput {
    units: Units,
    cs: Vec<f32>,
}

pub struct ParseUnits;

impl<R> SectionParser<R> for ParseUnits
where
    R: std::io::BufRead,
{
    type Output = ParseOutput;
    type Error = MyError;
    fn is_section_start(&self, line: &str) -> bool {
        line.split_ascii_whitespace()
            .nth(3)
            .map(|word| word == "Units:")
            .unwrap_or(false)
    }

    fn parse_section(
        &self,
        source: &mut ParseSource<R>,
        output: &mut Self::Output,
    ) -> Result<(), Self::Error> {
        let line = source.next().transpose()?.unwrap();

        let mut columns = line.split_ascii_whitespace();

        for _ in 0..4 {
            columns.next();
        }

        let Some(unit_str) = columns.next() else {
            return Err(MyError::UnexpectedEndOfLine);
        };

        match Units::from_str(unit_str) {
            Some(unit) => output.units = unit,
            None => return Err(MyError::UnexpectedEndOfLine),
        }

        Ok(())
    }
}

pub struct ParseCs;

impl<R> SectionParser<R> for ParseCs
where
    R: std::io::BufRead,
{
    type Output = ParseOutput;
    type Error = MyError;
    fn is_section_start(&self, line: &str) -> bool {
        line.starts_with("J=  0")
    }

    fn parse_section(
        &self,
        source: &mut ParseSource<R>,
        output: &mut Self::Output,
    ) -> Result<(), MyError> {
        // Skip the header
        let _ = source.nth(1);

        loop {
            if !source
                .peek()
                .map(|line| line.as_ref().is_ok_and(|line| line.starts_with("  0")))
                .unwrap_or(false)
            {
                break;
            }
            let line = source.next().transpose()?.unwrap();

            let mut columns = line.split_ascii_whitespace();

            _ = columns.next();

            let to = columns.next().ok_or(MyError::UnexpectedEndOfLine)?;
            let from = columns.next().ok_or(MyError::UnexpectedEndOfLine)?;

            if to == "s2S" && from == "<-s2S" {
                output.cs.push(
                    columns
                        .next()
                        .ok_or(MyError::UnexpectedEndOfLine)?
                        .parse()?,
                )
            }
        }

        Ok(())
    }
}

fn main() {
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .open("totalcs_1.0000E+02_J")
        .unwrap();

    let mut file_source = make_parse_source(&mut file);

    let mut parser = Parser::empty();
    let mut output = ParseOutput {
        units: Units::CentimetreSquared,
        cs: Vec::new(),
    };

    parser.add(ParseUnits);
    parser.add(ParseCs);

    parser.parse(&mut file_source, &mut output).unwrap();

    println!("{:?}", output);
}
