mod parser;
use parser::tokenizer::tokenize;
use parser::{parse, ParseResult};

fn main() {
    let tokens = tokenize("tests/test.py");
    let statements = parse(&tokens);
    match statements {
        ParseResult::Ok((statements, rest)) => {
            println!("Parsed result:");
            for statement in statements.iter() {
                println!("{:?}", statement);
            }
            if !rest.is_empty() {
                println!("Unparsed rest:");
                for token in rest.iter() {
                    println!("{:}", token);
                }
            }
        }
        ParseResult::Err => {
            println!("Parse error:");
            for token in tokens.iter() {
                println!("{:}", token);
            }
        }
    };
}
