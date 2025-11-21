pub struct IndentedWriter<W: std::io::Write> {
    writer: W,
    indent: usize,
}

impl<W: std::io::Write> IndentedWriter<W> {
    pub fn new(writer: W, indent: usize) -> Self {
        Self { writer, indent }
    }
}

// impl<WI: std::io::Write, W: std::io::Write + DerefMut<Target = WI>> IndentedWriter<W> {
//     pub fn as_dyn<'s>(&'s mut self) -> IndentedWriter<&'s mut dyn std::io::Write>
//     where
//         WI: 's,
//     {
//         IndentedWriter {
//             writer: self.writer.deref_mut() as &mut dyn std::io::Write,
//             indent: self.indent,
//         }
//     }
// }

impl<W: std::io::Write> IndentedWriter<W>
where
    for<'a> &'a mut W: std::io::Write,
{
    /// Temporarily add a further indent to the writer.
    ///
    /// Unfortunately, requires a higher-rank trait bound closure to guarantee the further indented writer does not get captured.
    ///
    /// # Example
    /// ```
    ///
    /// ```
    pub fn indent<O>(
        &mut self,
        indent: usize,
        f: impl for<'f> FnOnce(&mut IndentedWriter<W>) -> O,
    ) -> O {
        self.indent += indent;
        let result = f(self);
        self.indent -= indent;
        result
    }
}

impl<W: std::io::Write> std::io::Write for IndentedWriter<W> {
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut bytes_written = 0;
        let mut prev_idx = 0;
        let mut iter = buf.iter().enumerate();

        while let Some((idx, _)) = iter.find(|(_, &byte)| byte == b'\n') {
            self.writer.write_all(&buf[prev_idx..idx])?;
            self.writer.write_all(&[b'\n'])?;
            for _ in 0..self.indent {
                self.writer.write_all(&[b' '])?;
            }
            bytes_written += idx - prev_idx + 1;
            prev_idx = idx + 1;
        }

        if prev_idx < buf.len() {
            self.writer.write_all(&buf[prev_idx..])?;
            bytes_written += buf.len() - prev_idx;
        }

        Ok(bytes_written)
    }
}