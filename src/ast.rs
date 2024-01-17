#[derive(Debug)]
pub struct Program {
    pub function: Vec<TopLevelItem>,
}

pub trait Evaluate {
    fn evaluate(&self) -> i64;
}

#[derive(Debug)]
pub enum TopLevelItem {
    Function(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub return_type: TypeSpecifier,
    pub identifier: Identifier,
    pub arguments: Vec<Parameter>,
    pub statement: CompoundStatement,
}
impl FunctionDefinition {
    pub fn new(
        return_type: TypeSpecifier,
        identifier: Identifier,
        arguments: Vec<Parameter>,
        statement: CompoundStatement,
    ) -> Self {
        Self {
            return_type,
            identifier,
            arguments,
            statement,
        }
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub type_specifier: TypeSpecifier,
    pub identifier: Identifier,
}

impl Parameter {
    pub fn new(type_specifier: TypeSpecifier, identifier: Identifier) -> Self {
        Self {
            type_specifier,
            identifier,
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(VariableDeclaration),
}

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration(TypeSpecifier, Identifier),
    VariableDeclarationAssignment(TypeSpecifier, Identifier, Box<Expression>),
}

#[derive(Debug)]
pub enum If {
    SingleBranch(Box<Expression>, Box<Statement>), // if (expr) {statement}
    TwoBranch(Box<Expression>, Box<Statement>, Box<Statement>), // if (expr) {statement0} else {statement1}
}

#[derive(Debug)]
pub enum Statement {
    ReturnExpression(Box<Expression>),
    Expression(Box<Expression>),
    Assignment(Identifier, Box<Expression>),
    If(If),
    CompoundStatement(CompoundStatement),
    For(
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    ForDecl(
        VariableDeclaration,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    While(Box<Expression>, Box<Statement>),
    Do(Box<Statement>, Box<Expression>),
    Break,
    Continue,
    Empty,
}

#[derive(Debug)]
pub enum CompoundStatement {
    Empty,
    StatementList(Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Expression {
    LiteralNum(i64),
    UnaryOp(UnaryOp, Box<Expression>),
    Op(Box<Expression>, BinaryOperator, Box<Expression>),
    Variable(Identifier),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Assignment(Identifier, Box<Expression>),
    CompoundAssignment(Identifier, BinaryOperator, Box<Expression>),
    FunctionCall(Identifier, Vec<Box<Expression>>), // name+arguments
}

// Precedence: https://en.cppreference.com/w/c/language/operator_precedence
#[derive(Debug)]
pub enum BinaryOperator {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    LogicAnd,
    LogicOr,
    LogicEq,
    LogicNeq,
    GreaterThen,
    LessThen,
    Greq,
    Leq,
}

#[derive(Debug)]
pub enum TypeSpecifier {
    Char,
    Short,
    Int,
    Long,
    Signed,
    Unsigned,
    Float,
    Double,
    Void,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    type_specifier: TypeSpecifier,
    identifier: Identifier,
    expression: Option<Box<Expression>>,
}

impl VariableDeclaration {
    pub fn new(
        type_specifier: TypeSpecifier,
        identifier: Identifier,
        expression: Option<Box<Expression>>,
    ) -> Self {
        Self {
            type_specifier,
            identifier,
            expression,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    BitwiseNegation,
    LogicalNegation,
    PlusPlus,
    MinusMinus,
}
