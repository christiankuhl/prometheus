use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as ReplResult};

mod parser;
use parser::tokenizer::{tokenize_file, tokenize_line};
use parser::{parse, ParseResult};
mod debug;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        run_repl();
    } else {
        run_script(&args[1])?
    }
    Ok(())
}

fn run_script(filename: &str) -> Result<(), String> {
    let tokens = tokenize_file(filename)?;
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

fn run_repl() -> ReplResult<()> {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    let mut tokens = vec![];
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tok = tokenize_line(line.as_str());
                tokens.extend(tok.unwrap());
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}
