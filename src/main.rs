use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as ReplResult};

use prometheus::*;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        let _ = run_repl();
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
                evaluate(statement);
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
    'outer: loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                let mut tokenizer = Tokenizer::new().expect("Could not build tokenizer.");
                match tokenizer.tokenize(std::iter::once(line)) {
                    ParserState::Ok => {
                        if let Ok(tok) = tokenizer.extract() {
                            tokens.extend(tok);
                        }
                    }
                    ParserState::Error(msg) => {
                        println!("{}", msg);
                        continue;
                    }
                    ParserState::ContinuationNeeded => {
                        loop {
                            let continuation = rl.readline("... ");
                            match continuation {
                                Ok(line) => {
                                    if line.as_str() == "" {
                                        if let Ok(tok) = tokenizer.finalize() {
                                            tokens.extend(tok);
                                        }
                                        break;
                                    } 
                                    match tokenizer.tokenize(std::iter::once(line)) {
                                    ParserState::Error(msg) => {
                                        println!("{}", msg);
                                        break;
                                    }
                                    _ => {
                                        continue;
                                    }
                                }},
                                Err(ReadlineError::Interrupted) => {
                                    break 'outer;
                                }
                                Err(ReadlineError::Eof) => {
                                    break 'outer;
                                }
                                Err(err) => {
                                    println!("Error: {:?}", err);
                                    break 'outer;
                                }
                            }
                        }
                    }
                }
                println!("{:?}", &tokens);
                let ast = parse_interactive(&tokens);
                println!("{:?}", ast);
                match ast {
                    ParseResult::Err => {
                        println!("Parse error...");
                    }
                    ParseResult::Ok((stmts, _)) => {
                        for stmt in stmts.iter() {
                            evaluate(stmt);
                        }
                    }
                }
                tokens.clear();
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
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
