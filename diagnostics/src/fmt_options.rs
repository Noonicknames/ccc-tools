use colored::{Color, Style};

use crate::LogLevel;

#[derive(Debug, Clone)]
pub struct LogFmtOptions {
    pub sublog_indent: usize,

    pub info_color: Color,
    pub warn_color: Color,
    pub error_color: Color,

    pub message_color: Color,
    pub message_style: Style,

    pub other_color: Color,
    pub other_style: Style,
}

impl LogFmtOptions {
    pub fn get_level_color(&self, level: LogLevel) -> Color {
        match level {
            LogLevel::Info => self.info_color,
            LogLevel::Warn => self.warn_color,
            LogLevel::Error => self.error_color,
        }
    }
}

impl Default for LogFmtOptions {
    fn default() -> Self {
        Self {
            sublog_indent: 4,

            info_color: Color::Blue,
            warn_color: Color::Yellow,
            error_color: Color::Red,

            message_color: Color::BrightWhite,
            message_style: Style::default().bold(),

            other_color: Color::Blue,
            other_style: Style::default(),
        }
    }
}
