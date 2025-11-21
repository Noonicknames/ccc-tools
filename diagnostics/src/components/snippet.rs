use colored::{Color, Colorize};
use unicode_width::UnicodeWidthStr;

use crate::LogFmtOptions;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub enum FooterKind {
    #[default]
    Note,
    Help,
}

impl FooterKind {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Help => "help",
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct Footer {
    kind: FooterKind,
    content: String,
}

impl Footer {
    pub fn new(content: String, kind: FooterKind) -> Self {
        Self { content, kind }
    }
}

pub struct Snippet {
    source: Option<String>,
    chunks: Vec<SnippetChunk>,
    footers: Vec<Footer>,
}

impl Snippet {
    pub fn empty(source: Option<String>) -> Self {
        Self {
            source,
            chunks: Vec::new(),
            footers: Vec::new(),
        }
    }
    pub fn from_str(
        str: &str,
        source: Option<String>,
        start_line: usize,
        kind: SnippetLineKind,
    ) -> Self {
        Self {
            source,
            chunks: vec![SnippetChunk::from_str(str, kind, start_line)],
            footers: Vec::new(),
        }
    }
    pub fn add_chunk(&mut self, chunk: SnippetChunk) {
        self.chunks.push(chunk);
    }
    pub fn with_chunk(mut self, chunk: SnippetChunk) -> Self {
        self.add_chunk(chunk);
        self
    }
    pub fn add_footer(&mut self, footer: Footer) {
        self.footers.push(footer);
    }
    pub fn with_footer(mut self, footer: Footer) -> Self {
        self.add_footer(footer);
        self
    }
}

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SnippetLineKind {
    #[default]
    Normal,
    Modified,
    Added,
    Removed,
}

impl SnippetLineKind {
    pub const fn margin_char(&self) -> char {
        match self {
            Self::Normal => '|',
            Self::Modified => '~',
            Self::Added => '+',
            Self::Removed => '-',
        }
    }
    pub const fn color(&self, options: &LogFmtOptions) -> Color {
        match self {
            Self::Normal => options.other_color,
            Self::Modified => Color::Yellow,
            Self::Added => Color::Green,
            Self::Removed => Color::Red,
        }
    }
}

#[derive(Debug, Default)]
pub struct SnippetLine {
    pub content: String,
    pub highlight: Option<LineHighlight>,
    pub kind: SnippetLineKind,
}

impl SnippetLine {
    pub fn new(content: String, kind: SnippetLineKind) -> Self {
        Self {
            content,
            highlight: None,
            kind,
        }
    }
    pub fn new_with_highlight(
        content: String,
        kind: SnippetLineKind,
        highlight: Option<LineHighlight>,
    ) -> Self {
        Self {
            content,
            highlight,
            kind,
        }
    }
    pub fn with_content(mut self, content: String) -> Self {
        self.content = content;
        self
    }
    pub fn with_highlight(mut self, highlight: Option<LineHighlight>) -> Self {
        self.highlight = highlight;
        self
    }
    pub fn add_highlight_auto(
        &mut self,
        highlighted_substr: &str,
        comment: String,
        theme: LineHighlightTheme,
    ) {
        self.highlight = LineHighlight::new_auto(&self.content, highlighted_substr, comment, theme);
    }
    pub fn with_highlight_auto(
        mut self,
        highlighted_substr: &str,
        comment: String,
        theme: LineHighlightTheme,
    ) -> Self {
        self.add_highlight_auto(highlighted_substr, comment, theme);
        self
    }
}

/// A contiguous chunk of code or other text file.
pub struct SnippetChunk {
    start_line: usize,
    lines: Vec<SnippetLine>,
}

impl SnippetChunk {
    pub fn empty(start_line: usize) -> Self {
        Self {
            start_line,
            lines: Vec::new(),
        }
    }
    pub fn from_str(str: &str, kind: SnippetLineKind, start_line: usize) -> Self {
        Self {
            start_line,
            lines: str
                .lines()
                .map(|line| SnippetLine::new(line.to_string(), kind))
                .collect(),
        }
    }
    pub fn add_str(&mut self, str: &str, kind: SnippetLineKind) {
        str.lines()
            .map(|line| SnippetLine::new(line.to_string(), kind))
            .for_each(|line| self.lines.push(line));
    }
    pub fn with_str(mut self, str: &str, kind: SnippetLineKind) -> Self {
        self.add_str(str, kind);
        self
    }
    pub fn add_line(&mut self, line: SnippetLine) {
        self.lines.push(line);
    }
    pub fn with_line(mut self, line: SnippetLine) -> Self {
        self.add_line(line);
        self
    }
    pub fn highest_line_num(&self) -> usize {
        self.start_line + self.lines.len()
    }
    pub fn line_nums_lines(&self) -> impl Iterator<Item = (usize, &str)> {
        (self.start_line..self.start_line + self.lines.len())
            .zip(self.lines.iter().map(|line| line.content.as_str()))
    }
    pub fn set_highlight(
        &mut self,
        line_num: usize,
        mut highlight: Option<LineHighlight>,
    ) -> Result<Option<LineHighlight>, Option<LineHighlight>> {
        if let Some(line) = self.lines.get_mut(line_num - self.start_line) {
            std::mem::swap(&mut highlight, &mut line.highlight);
            Ok(highlight)
        } else {
            Err(highlight)
        }
    }
    pub fn set_highlight_auto(
        &mut self,
        line_num: usize,
        highlighted_substr: &str,
        comment: String,
        theme: LineHighlightTheme,
    ) -> Result<Option<LineHighlight>, String> {
        if let Some(line) = self.lines.get_mut(line_num - self.start_line) {
            let mut highlight = Some(
                LineHighlight::new_auto(&line.content, highlighted_substr, comment, theme).unwrap(),
            );
            std::mem::swap(&mut highlight, &mut line.highlight);
            Ok(highlight)
        } else {
            Err(comment)
        }
    }
    pub fn with_highlight(mut self, line: usize, highlight: LineHighlight) -> Self {
        self.set_highlight(line, Some(highlight)).unwrap();
        self
    }
    pub fn with_highlight_auto(
        mut self,
        line_num: usize,
        highlighted_substr: &str,
        comment: String,
        theme: LineHighlightTheme,
    ) -> Self {
        self.set_highlight_auto(line_num, highlighted_substr, comment, theme)
            .unwrap();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineHighlightTheme {
    highlight_char: char,
    color: Color,
}

impl LineHighlightTheme {
    pub const ERROR: Self = Self {
        highlight_char: '^',
        color: Color::Red,
    };
    pub const INFO: Self = Self {
        highlight_char: '-',
        color: Color::Blue,
    };
}

#[derive(Debug)]
pub struct LineHighlight {
    start_offset: usize,
    length: usize,
    comment: String,
    theme: LineHighlightTheme,
}

impl LineHighlight {
    pub fn new(
        start_offset: usize,
        length: usize,
        comment: String,
        theme: LineHighlightTheme,
    ) -> Self {
        Self {
            start_offset,
            length,
            comment,
            theme,
        }
    }
    pub fn new_auto(
        content: &str,
        highlighted_substr: &str,
        comment: String,
        theme: LineHighlightTheme,
    ) -> Option<Self> {
        let Some(start_offset) = content
            .find(highlighted_substr)
            .map(|byte_index| content[0..byte_index].width())
        else {
            return None;
        };

        Some(Self {
            start_offset,
            length: highlighted_substr.width(),
            comment,
            theme,
        })
    }
}

#[derive(Debug)]
pub struct DefaultSnippetRenderer;

impl DefaultSnippetRenderer {
    pub fn write_chunk(
        &self,
        chunk: &SnippetChunk,
        options: &LogFmtOptions,
        line_num_width: usize,
        output: &mut dyn std::io::Write,
    ) -> std::io::Result<()> {
        for (line_num, line) in
            (chunk.start_line..chunk.start_line + chunk.lines.len()).zip(chunk.lines.iter())
        {
            write!(
                output,
                "{} {} {}\n",
                format!("{:>line_num_width$}", line_num)
                    .color(options.other_color)
                    .bold(),
                line.kind
                    .margin_char()
                    .encode_utf8(&mut [0; 4])
                    .color(line.kind.color(options))
                    .bold(),
                line.content,
            )?;
            if let Some(highlight) = &line.highlight {
                write!(
                    output,
                    "{} {:comment_whitespace$}{}\n",
                    format!("{:line_num_width$} |", "")
                        .color(options.other_color)
                        .bold(),
                    "",
                    format!(
                        "{} {}",
                        highlight
                            .theme
                            .highlight_char
                            .encode_utf8(&mut [0; 4])
                            .repeat(highlight.length),
                        highlight.comment,
                    )
                    .color(highlight.theme.color)
                    .bold(),
                    comment_whitespace = highlight.start_offset,
                )?;
            }
        }
        Ok(())
    }

    pub fn write(
        &self,
        snippet: &Snippet,
        options: &LogFmtOptions,
        output: &mut dyn std::io::Write,
    ) -> std::io::Result<()> {
        let largest_line_num = snippet
            .chunks
            .iter()
            .map(|chunk| chunk.highest_line_num())
            .max()
            .unwrap_or(0);
        let line_num_width = largest_line_num.ilog10().max(1) as usize + 1;

        if let Some(source) = &snippet.source {
            write!(
                output,
                "{:line_num_width$}{} {}\n",
                "",
                "-->".color(options.other_color).bold(),
                source,
            )?;
            write!(
                output,
                "{}\n",
                format!("{:line_num_width$} |", "")
                    .color(options.other_color)
                    .bold(),
            )?;
        }

        if let Some(last_chunk) = snippet.chunks.last() {
            for chunk in snippet.chunks[0..snippet.chunks.len() - 1].iter() {
                self.write_chunk(chunk, options, line_num_width, output)?;
                write!(output, "{}\n", "...".color(options.other_color).bold(),)?;
            }
            self.write_chunk(&last_chunk, options, line_num_width, output)?;
        }

        for footer in snippet.footers.iter() {
            write!(
                output,
                "{} {}: {}\n",
                format!("{:line_num_width$} =", "")
                    .color(options.other_color)
                    .bold(),
                footer.kind.name().bold(),
                footer.content,
            )?;
        }

        Ok(())
    }
}
