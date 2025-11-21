use std::io::BufReader;

pub struct Parser<O, R, E>
where
    R: std::io::BufRead,
{
    sections: Vec<Box<dyn SectionParser<R, Output = O, Error = E>>>,
}

impl<O, R, E> Parser<O, R, E>
where
    R: std::io::BufRead,
{
    pub const fn empty() -> Self {
        Self {
            sections: Vec::new(),
        }
    }
    pub fn add(&mut self, section: impl SectionParser<R, Output = O, Error = E> + 'static) {
        self.sections.push(Box::new(section));
    }
}

impl<O, R, E> Parser<O, R, E>
where
    R: std::io::BufRead,
    E: From<std::io::Error>,
{
    pub fn parse(&self, source: &mut ParseSource<R>, output: &mut O) -> Result<(), E> {
        'outer: for section in self.sections.iter() {
            'inner: loop {
                let line = match source.peek() {
                    Some(Ok(line)) => line,
                    Some(Err(_)) => {
                        return Err(source.next().transpose().unwrap_err().into());
                    }
                    None => break 'outer,
                };
                if section.is_section_start(line) {
                    break 'inner;
                }

                source.next();
            }
            section.parse_section(source, output)?;
        }
        Ok(())
    }
}

pub struct Match<O, R, E>
where
    R: std::io::BufRead,
{
    sections: Vec<Box<dyn SectionParser<R, Output = O, Error = E>>>,
}

impl<O, R, E> SectionParser<R> for Match<O, R, E>
where
    R: std::io::BufRead,
    E: From<std::io::Error>,
{
    type Output = O;
    type Error = E;
    fn is_section_start(&self, line: &str) -> bool {
        self.sections
            .iter()
            .find(|section| section.is_section_start(line))
            .is_some()
    }
    fn parse_section(
        &self,
        source: &mut ParseSource<R>,
        output: &mut Self::Output,
    ) -> Result<(), E> {
        let line = match source.peek() {
            Some(Ok(line)) => line,
            Some(Err(_)) => unsafe {
                return Err(source.next().unwrap_unchecked().unwrap_err().into());
            },
            None => return Ok(()),
        };
        let Some(section) = self
            .sections
            .iter()
            .find(|section| section.is_section_start(line))
        else {
            return Ok(());
        };

        section.parse_section(source, output)
    }
}

pub trait SectionParser<R>
where
    R: std::io::BufRead,
{
    type Output;
    type Error;
    fn is_section_start(&self, line: &str) -> bool;
    fn parse_section(
        &self,
        source: &mut ParseSource<R>,
        output: &mut Self::Output,
    ) -> Result<(), Self::Error>;
}

pub fn make_parse_source<R>(source: R) -> ParseSource<BufReader<R>>
where
    R: std::io::Read,
{
    make_parse_source_buffered(BufReader::new(source))
}

pub fn make_parse_source_buffered<R>(source: R) -> ParseSource<R>
where
    R: std::io::BufRead,
{
    source.lines().peekable()
}

pub type ParseSource<R> = std::iter::Peekable<std::io::Lines<R>>;
