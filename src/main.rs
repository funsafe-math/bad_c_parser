use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;

use lalrpop_util::lexer::Token;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
mod ast;
mod compile;

lalrpop_mod!(pub c); // synthesized by LALRPOP

fn print_souce_location(contents: &str, location: usize) {
    let mut from_start = 0;
    for line in contents.split("\n") {
        if from_start <= location && location < (from_start + line.len()) {
            let line_position = location - from_start;
            println!("{}", line);
            println!("{}^", " ".repeat(line_position))
        }
        from_start += line.len() + 1; // +1 for newline
    }
}

fn print_error(err: ParseError<usize, Token, &str>, code: &str) {
    match err {
        lalrpop_util::ParseError::InvalidToken { location } => {
            println!("Parse error: Invalid token at {}", location);
            print_souce_location(&code, location);
        }
        lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
            println!("Parse error: Unrecognized end of file at {}", location);
            print_souce_location(&code, location);
            println!("Exprected one of:");
            for e in &expected {
                print!("- {}", e);
            }
            println!("");
        }
        lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
            let (begin, token, _len) = token;
            println!("Parse error: UnrecognizedToken '{}'", token);
            print_souce_location(&code, begin);
            println!("Exprected one of:");
            for e in &expected {
                print!("- {}", e);
            }
            println!("");
        }
        lalrpop_util::ParseError::ExtraToken { token } => {
            let (begin, token, _len) = token;
            println!("Parse error: Extra token at {}", begin);
            println!("Got token {}", token);
            print_souce_location(&code, begin);
        }
        lalrpop_util::ParseError::User { error } => {
            println!("Parse error: {}", error);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} <input.c> <output.s>", args[0]);
        return;
    }
    let filepath = &args[1];
    let output_path = &args[2];
    let contents = fs::read_to_string(filepath);
    if let Ok(code) = contents {
        parse(code, output_path, filepath);
    } else {
        eprintln!("Failed to open {}", filepath);
    }
}

fn parse(code: String, output_path: &String, filepath: &String) {
    let parsed = c::ProgramParser::new().parse(&code);
    match parsed {
        Ok(parsed) => {
            println!("Parsed: {:#?}", &parsed);
            let asm = parsed.compile();
            match asm {
                Err(err) => {
                    println!("Parse error: {}", err);
                    return;
                },
                Ok(_) => {},
            }
            let asm = asm.unwrap();
            // println!("Generated asm: \n{}", asm);
            let file = File::create(output_path);
            match file {
                Ok(mut file) => {
                    let result = file.write_all(asm.as_bytes());
                    match result {
                        Ok(_ok) => {}
                        Err(err) => println!("Failed to save to file, because: {}", err),
                    }
                }
                Err(error) => {
                    println!("Failed to open file, because: {}", error)
                }
            }
        }
        Err(err) => {
            println!("Failed to parse file {}", filepath);
            print_error(err, &code);
        }
    }
}

#[test]
fn check_parser() {
    assert!(c::ExpressionParser::new().parse("1 * 2 + 3*2 - 5").is_ok());
    assert!(c::ExpressionParser::new()
        .parse("1 * 2 + 3*2 - 5 -")
        .is_err());

    assert!(c::ParameterListParser::new()
        .parse("int argc, char argv, long ptr")
        .is_ok());

    assert!(c::StatementParser::new().parse("abc = 1;").is_ok());

    assert!(c::StatementParser::new().parse("abc = 1+2+3;").is_ok());

    assert!(c::IdentifierParser::new().parse("abcd").is_ok());

    assert!(c::IdentifierParser::new().parse("x").is_ok());

    assert!(c::CompoundStatementParser::new()
        .parse("{ int x ; abc = 1; xyz = 2; }")
        .is_ok());

    assert!(c::FunctionDefinitionParser::new()
        .parse("int main(int argc, int argv){ int x =1; int y; abc = 1; xyz = 2; return 1+2+y; }")
        .is_ok());

    assert!(c::IfParser::new()
        .parse("if (1 + 2 == 4) { return 1;}")
        .is_ok());

    assert!(c::IfParser::new()
        .parse("if (1 + 2 == 4) return 1;")
        .is_ok());

    assert!(c::IfParser::new()
        .parse("if (1 + 2 == 4){ return 1;} else return 2;")
        .is_ok());

    assert!(c::ExpressionParser::new().parse("1 == 1 ? x : y").is_ok());

    assert!(c::StatementParser::new().parse("{{{{{}}}}}").is_ok());

    assert!(c::StatementParser::new()
        .parse("{i = 1;{{{y=2;{}}}x = 5;}}")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("for(int i = 1; i < 10;++i){return 1;}")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("for(i = 1; i < 10;++i){return 1;}")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("while (i == 1) {return 1;}")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("do { i+= 1; } while (1);")
        .is_ok());
    assert!(c::ProgramParser::new().parse("int main() {a;}").is_ok());

    assert!(c::ProgramParser::new()
        .parse("int main() { main(1,2,3,4); }")
        .is_ok());
    //    assert!(c::TermParser::new().parse("22").is_ok());
    //    assert!(c::TermParser::new().parse("(22)").is_ok());
    //    assert!(c::TermParser::new().parse("((((22))))").is_ok());
    //    assert!(c::TermParser::new().parse("((22)").is_err());
}
