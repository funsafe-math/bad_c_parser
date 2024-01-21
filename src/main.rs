use lalrpop_util::lalrpop_mod;

use std::fs;
use std::fs::File;
use std::io::Write;
mod ast;
mod evalue;

lalrpop_mod!(pub c); // synthesized by LALRPOP
                     //
#[test]
fn calculator1() {
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

fn print_souce_location(contents: &str, location: usize){
    // contents.find(pat)
    // let (before, after) = contents.split_at(location);
    // println!("{}", before);
    let mut from_start = 0;
    for line in contents.split("\n") {
        if from_start <= location && location < (from_start + line.len()){
            let line_position = location - from_start;
            println!("{}", line);
            println!("{}^", " ".repeat(line_position))
        }
        from_start += line.len() + 1; // +1 for newline
    }

}

fn main() {
    let filepath = "/home/wojciech/projects/studia/metody-i-algorytmy-kompilacji/projekt/parser_project/output/example.c";
    let contents = fs::read_to_string(filepath);
    if let Ok(code) = contents {
        let parsed = c::ProgramParser::new().parse(&code);
        match parsed {
            Ok(parsed) => {
                println!("Parsed: {:#?}", &parsed);
                let asm: String = parsed.compile();
                println!("Generated asm: \n{}", asm);
                let file = File::create("/home/wojciech/projects/studia/metody-i-algorytmy-kompilacji/projekt/parser_project/output/output.s");
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
                match err {
                    lalrpop_util::ParseError::InvalidToken { location } => {
                        println!("Parse error: Invalid token at {}", location);
                        print_souce_location(&code, location);

                    },
                    lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                        println!("Parse error: Unrecognized end of file at {}", location);
                        print_souce_location(&code, location);
                        println!("Exprected one of:");
                        for e in &expected {
                            print!("- {}", e);
                        }
                        println!("");

                    },
                    lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                        let (begin, token, _len) = token;
                        println!("Parse error: UnrecognizedToken '{}'", token);
                        print_souce_location(&code, begin);
                        println!("Exprected one of:");
                        for e in &expected {
                            print!("- {}", e);
                        }
                        println!("");
                    },
                    lalrpop_util::ParseError::ExtraToken { token } => {
                        let (begin, token, _len) = token;
                        println!("Parse error: Extra token at {}", begin);
                        println!("Got token {}", token);
                        print_souce_location(&code, begin);
                    },
                    lalrpop_util::ParseError::User { error } => {
                        println!("Parse error: {}", error);
                    },
                }
            }
        }
    } else {
        println!("Failed to open {}", filepath);
    }

    // println!(
    //     "Parsing expression: {:#?}",
    //     c::ExpressionParser::new().parse("1 == 2 + 5 && 2 != 5*2")
    // );

    // check_parser_value("int main(int argc, char argv) { return 1; }");

    // println!("{:#?}", c::StatementListParser::new()
    //     .parse("int xyz ; abc = 1; xyz = 2;")
    //     );
}
