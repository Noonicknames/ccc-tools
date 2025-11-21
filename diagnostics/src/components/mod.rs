use crate::{
    components::snippet::{DefaultSnippetRenderer, Snippet},
    LogFmtOptions,
};

pub mod snippet;

pub enum LogComponent {
    Snippet(Snippet),
}

#[derive(Debug)]
pub struct DefaultLogComponentRenderer {
    snippet_renderer: DefaultSnippetRenderer,
}

impl DefaultLogComponentRenderer {
    pub fn new() -> Self {
        Self {
            snippet_renderer: DefaultSnippetRenderer,
        }
    }
    /// Write out log message to the given writer.
    pub fn write(
        &self,
        component: &LogComponent,
        options: &LogFmtOptions,
        output: &mut dyn std::io::Write,
    ) -> std::io::Result<()> {
        match component {
            LogComponent::Snippet(snippet) => self.snippet_renderer.write(snippet, options, output),
        }
    }
}
