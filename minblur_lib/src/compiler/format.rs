use std::fmt;

pub trait FormatCli {
    /// Format this object for human-readable output
    fn format_cli(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

// auto-impl for types implementing Display
impl<T: fmt::Display> FormatCli for T {
    fn format_cli(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(feature = "json")]
pub use format_json::*;
#[cfg(all(feature = "json", feature = "serialize"))]
mod format_json {
    use serde_json::Value;

    pub trait FormatJson {
        fn format_json(&self) -> Value;
    }

    // auto-impl for types implementing serde::Serialize
    impl<T: serde::Serialize> FormatJson for T {
        fn format_json(&self) -> Value {
            serde_json::to_value(&self).unwrap()
        }
    }
}
