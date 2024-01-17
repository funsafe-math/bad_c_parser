use lalrpop_util::lalrpop_mod;

use crate::calculator1::ExpressionParser;
use crate::calculator1::FunctionDefinitionParser;
use crate::calculator1::IdentifierParser;
use crate::calculator1::IfParser;
use crate::calculator1::StatementParser;
mod ast;

lalrpop_mod!(pub calculator1); // synthesized by LALRPOP
                               //
#[test]
fn calculator1() {
    assert!(calculator1::ExpressionParser::new()
        .parse("1 * 2 + 3*2 - 5")
        .is_ok());
    assert!(calculator1::ExpressionParser::new()
        .parse("1 * 2 + 3*2 - 5 -")
        .is_err());

    assert!(calculator1::ParameterListParser::new()
        .parse("int argc, char argv, long ptr")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("abc = 1;")
        .is_ok());
    //   assert!(calculator1::StatementParser::new()
    //       .parse("int abc;")
    //       .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("abc = 1+2+3;")
        .is_ok());

    assert!(calculator1::IdentifierParser::new().parse("abcd").is_ok());

    assert!(calculator1::IdentifierParser::new().parse("x").is_ok());

    //   assert!(calculator1::StatementListParser::new()
    //       .parse("int x ; abc = 1; xyz = 2;")
    //       .is_ok());
    //   assert!(calculator1::CompoundStatementParser::new()
    //       .parse("{ int x ; abc = 1; xyz = 2; }")
    //       .is_ok());
    //
    //   assert!(calculator1::FunctionDefinitionParser::new()
    //       .parse("int main(int argc, int argv){ int x =1; int y; abc = 1; xyz = 2; return 1+2+y; }")
    //       .is_ok());

    //   assert!(calculator1::IfParser::new()
    //       .parse("if (1 + 2 == 4) { return 1;}")
    //       .is_ok());

    assert!(calculator1::IfParser::new()
        .parse("if (1 + 2 == 4) return 1;")
        .is_ok());

    assert!(calculator1::IfParser::new()
        .parse("if (1 + 2 == 4){ return 1;} else return 2;")
        .is_ok());

    assert!(calculator1::ExpressionParser::new()
        .parse("1 == 1 ? x : y")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("{{{{{}}}}}")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("{i = 1;{{{y=2;{}}}x = 5;}}")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("for(int i = 1; i < 10;++i){return 1;}")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("for(i = 1; i < 10;++i){return 1;}")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("while (i == 1) {return 1;}")
        .is_ok());

    assert!(calculator1::StatementParser::new()
        .parse("do { i+= 1; } while (1);")
        .is_ok());
    assert!(calculator1::ProgramParser::new()
        .parse("int main() {a;}")
        .is_ok());

    assert!(calculator1::ProgramParser::new()
        .parse("int main() { main(1,2,3,4); }")
        .is_ok());
    //    assert!(calculator1::TermParser::new().parse("22").is_ok());
    //    assert!(calculator1::TermParser::new().parse("(22)").is_ok());
    //    assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
    //    assert!(calculator1::TermParser::new().parse("((22)").is_err());
}

fn main() {
    // println!(
    //     "Parsing literal expression: {:#?}",
    //     calculator1::ExpressionParser::new().parse("1")
    // );
    // println!(
    //     "Parsing literal hex expression: {:#?}",
    //     calculator1::ExpressionParser::new().parse("0xbadbeef")
    // );
    // println!(
    //     "Parsing literal bin expression: {:#?}",
    //     calculator1::ExpressionParser::new().parse("0b1111")
    // );
    // println!(
    //     "Parsing statement: {:#?}",
    //     calculator1::StatementParser::new().parse("return 1;")
    // );
    // println!(
    //     "Parsing identifier: {:#?}",
    //     calculator1::IdentifierParser::new().parse("Bad_apples123abc")
    // );
    println!(
        "Parsing function: {:#?}",
        calculator1::ProgramParser::new().parse("void foo() {} int main() { main(1,2,3,4); }")
    );

    println!(
        "Parsing function: {:#?}",
        calculator1::ProgramParser::new().parse("
            int fib(int n) {
                if (n == 0 || n == 1) {
                    return n;
                } else {
                    return fib(n - 1) + fib(n - 2);
                }
            }

            int main() {
                int n = 10;
                return fib(n);
            }
    ")
    );

    // println!(
    //     "Parsing expression: {:#?}",
    //     calculator1::ExpressionParser::new().parse("1 == 2 + 5 && 2 != 5*2")
    // );

    // check_parser_value("int main(int argc, char argv) { return 1; }");

    // println!("{:#?}", calculator1::StatementListParser::new()
    //     .parse("int xyz ; abc = 1; xyz = 2;")
    //     );
}
