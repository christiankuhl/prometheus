use super::locations::Span;

#[derive(Debug)]
pub struct Error(Span, String);

impl Error {
    pub fn new(span: Span, msg: &str) -> Self {
        Self(span, msg.to_string())
    }
    pub fn starting_from(span: Span, msg: &str) -> Self {
        Self(span, msg.to_string())
    }
    pub fn with_line(span: Span, msg: &str) -> Self {
        Self(span, msg.to_string())
    }
    pub fn with_range(start_span: Span, end_span: Span, msg: &str) -> Self {
        let span = start_span.till(&end_span);
        Self(span, msg.to_string())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error: {} in {:?}",
            self.1, self.0,
        )
    }
}
