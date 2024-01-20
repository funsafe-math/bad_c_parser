#[derive(Debug, Clone)]
pub struct Program {
    pub top_level_items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Function(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
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
    // How much stack space needs to be allocated
    pub fn required_stack(&self) -> usize {
        match &self.statement {
            CompoundStatement::Empty => 0,
            CompoundStatement::StatementList(list) => {
                let mut ret = 0;
                for block_item in list {
                    match block_item {
                        BlockItem::Statement(_) => {
                            // TODO: implement
                        }
                        BlockItem::Declaration(decl) => {
                            ret += decl.type_specifier.size();
                        }
                    }
                }
                ret
            }
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration(TypeSpecifier, Identifier),
    VariableDeclarationAssignment(TypeSpecifier, Identifier, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum If {
    SingleBranch(Box<Expression>, Box<Statement>), // if (expr) {statement}
    TwoBranch(Box<Expression>, Box<Statement>, Box<Statement>), // if (expr) {statement0} else {statement1}
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum CompoundStatement {
    Empty,
    StatementList(Vec<BlockItem>),
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

impl TypeSpecifier {
    pub fn size(&self) -> usize {
        match self {
            TypeSpecifier::Char => todo!(),
            TypeSpecifier::Short => todo!(),
            TypeSpecifier::Int => 4,
            TypeSpecifier::Long => 8,
            TypeSpecifier::Signed => todo!(),
            TypeSpecifier::Unsigned => todo!(),
            TypeSpecifier::Float => todo!(),
            TypeSpecifier::Double => todo!(),
            TypeSpecifier::Void => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub type_specifier: TypeSpecifier,
    pub identifier: Identifier,
    pub expression: Option<Box<Expression>>,
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Minus,
    BitwiseNegation,
    LogicalNegation,
    PlusPlus,
    MinusMinus,
}
