#![allow(unused_variables, dead_code)]
#![recursion_limit = "1024"]

mod parser;
pub use parser::tokenizer::{tokenize_file, LexerState, Tokenizer};
pub use parser::{parse, parse_interactive, ParseResult};

mod interpreter;
pub use interpreter::evaluate;
