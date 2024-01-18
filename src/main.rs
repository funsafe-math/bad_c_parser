use crate::c::ExpressionParser;
use crate::c::FunctionDefinitionParser;
use crate::c::IdentifierParser;
use crate::c::IfParser;
use crate::c::StatementParser;
use lalrpop_util::lalrpop_mod;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
mod ast;
mod evalue;
use evalue::*;

lalrpop_mod!(pub c); // synthesized by LALRPOP
                               //
#[test]
fn calculator1() {
    assert!(c::ExpressionParser::new()
        .parse("1 * 2 + 3*2 - 5")
        .is_ok());
    assert!(c::ExpressionParser::new()
        .parse("1 * 2 + 3*2 - 5 -")
        .is_err());

    assert!(c::ParameterListParser::new()
        .parse("int argc, char argv, long ptr")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("abc = 1;")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("abc = 1+2+3;")
        .is_ok());

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

    assert!(c::ExpressionParser::new()
        .parse("1 == 1 ? x : y")
        .is_ok());

    assert!(c::StatementParser::new()
        .parse("{{{{{}}}}}")
        .is_ok());

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
    assert!(c::ProgramParser::new()
        .parse("int main() {a;}")
        .is_ok());

    assert!(c::ProgramParser::new()
        .parse("int main() { main(1,2,3,4); }")
        .is_ok());
    //    assert!(c::TermParser::new().parse("22").is_ok());
    //    assert!(c::TermParser::new().parse("(22)").is_ok());
    //    assert!(c::TermParser::new().parse("((((22))))").is_ok());
    //    assert!(c::TermParser::new().parse("((22)").is_err());
}

fn main() {
    // println!(
    //     "Parsing literal expression: {:#?}",
    //     c::ExpressionParser::new().parse("1")
    // );
    // println!(
    //     "Parsing literal hex expression: {:#?}",
    //     c::ExpressionParser::new().parse("0xbadbeef")
    // );
    // println!(
    //     "Parsing literal bin expression: {:#?}",
    //     c::ExpressionParser::new().parse("0b1111")
    // );
    // println!(
    //     "Parsing statement: {:#?}",
    //     c::StatementParser::new().parse("return 1;")
    // );
    // println!(
    //     "Parsing identifier: {:#?}",
    //     c::IdentifierParser::new().parse("Bad_apples123abc")
    // );
    // println!(
    //     "Parsing function: {:#?}",
    //     c::ProgramParser::new().parse("void foo() {} int main() { main(1,2,3,4); }")
    // );

    // let parsed = c::ProgramParser::new().parse(
    //     "
    //         int fib(int n) {
    //             if (n == 0 || n == 1) {
    //                 print(1);
    //                 return n;
    //             } else {
    //                 return fib(n - 1) + fib(n - 2);
    //             }
    //         }

    //         int main() {
    //             int n = 10;
    //             return fib(n);
    //         }
    // ",
    // );

    // println!("Parsing function: {:#?}", &parsed);

    // if let Ok(result) = &parsed {
    //     result;
    // }

    // let args: Vec<String> = env::args().collect();
    // dbg!(args);

    let filepath = "/home/wojciech/projects/studia/metody-i-algorytmy-kompilacji/projekt/parser_project/output/example.c";
    let contents = fs::read_to_string(filepath);
    if let Ok(code) = contents {
        let parsed = c::ProgramParser::new().parse(&code);
        if let Ok(parsed) = &parsed {
            println!("Parsed: {:#?}", &parsed);
            let asm: String = parsed.compile();
            println!("Generated asm: \n{}", asm);
            let mut file = File::create("/home/wojciech/projects/studia/metody-i-algorytmy-kompilacji/projekt/parser_project/output/output.s");
            match file {
                Ok(mut file) => {
                    let result = file.write_all(asm.as_bytes());
                    match result {
                        Ok(ok) => {}
                        Err(err) => println!("Failed to save to file, because: {}", err),
                    }
                }
                Err(error) => {
                    println!("Failed to open file, because: {}", error)
                }
            }
        } else {
            println!("Failed to parse file {}", filepath);
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
