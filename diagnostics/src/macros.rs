#[macro_export]
macro_rules! log {
    ($level: expr, $($tt: tt)+) => {
        $crate::Log {
            level: $level,
            message: format!($($tt)*).into(),
            components: Vec::new(),
            sublogs: Vec::new(),
        }
    };
}
pub use log;

#[macro_export]
macro_rules! info {
    ($($tt: tt)+) => {
        $crate::log!($crate::LogLevel::Info, $($tt)*)
    };
}
pub use info;

#[macro_export]
macro_rules! warn {
    ($($tt: tt)+) => {
        $crate::log!($crate::LogLevel::Warn, $($tt)*)
    };
}
pub use crate::warn;

#[macro_export]
macro_rules! error {
    ($($tt: tt)+) => {
        $crate::log!($crate::LogLevel::Error, $($tt)*)
    };
}
pub use error;