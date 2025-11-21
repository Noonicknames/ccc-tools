use std::io::stdout;

use diagnostics::{
    components::snippet::{
        LineHighlightTheme, Snippet, SnippetChunk, SnippetLine, SnippetLineKind,
    },
    diagnostics::{ChannelAllow, Diagnostics},
    macros::*,
    Log, LogComponent, LogFmtOptions,
};

fn main() {
    let mut diagnostics = Diagnostics::new(LogFmtOptions::default());
    diagnostics.add_channel(ChannelAllow::all(), stdout());
    diagnostics.write_log(&what_have_you_done_log()).unwrap();
}

fn what_have_you_done_log() -> Log {
    error!("What have you done...").with_component(LogComponent::Snippet(
        Snippet::empty(Some("src/🗿.py".to_string())).with_chunk(
            SnippetChunk::empty(1)
                .with_line(
                    SnippetLine::new("def 🗿():".to_string(), SnippetLineKind::Normal)
                        .with_highlight_auto(
                            "🗿",
                            "No way this is valid bro...".to_string(),
                            LineHighlightTheme::ERROR,
                        ),
                )
                .with_line(SnippetLine::new(
                    "    print(\"🗿\")".to_string(),
                    SnippetLineKind::Normal,
                ))
                .with_str("\nif __name__ == \"main\":", SnippetLineKind::Normal)
                .with_line(
                    SnippetLine::new("    🗿()".to_string(), SnippetLineKind::Normal)
                        .with_highlight_auto("🗿", "Nani!?".to_string(), LineHighlightTheme::ERROR),
                ),
        ),
    ))
}
