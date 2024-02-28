mod parser;
use parser::tokenizer::tokenize;
use parser::{parse, ParseResult};
mod debug;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        return Err("No input file provided".to_string())
    }
    let tokens = tokenize(&args[1])?;
    // for token in tokens.iter() {
    //     println!("{:}", token);
    // }
    let statements = parse(&tokens);
    match statements {
        ParseResult::Ok((statements, rest)) => {
            println!("\nParsed result:");
            for statement in statements.iter() {
                println!("\n{:?}", statement);
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
    Ok(())
}
