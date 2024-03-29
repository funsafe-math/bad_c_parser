#[derive(Debug, Clone)]
pub struct Program {
    pub top_level_items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Function(FunctionDefinition),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub return_type: TypeSpecifier,
    pub identifier: Identifier,
    pub arguments: Vec<Parameter>,
    pub variadic: bool,
}

impl FunctionDeclaration {
    pub fn new(
        return_type: TypeSpecifier,
        identifier: Identifier,
        arguments: Vec<Parameter>,
        variadic: bool,
    ) -> Self {
        Self {
            return_type,
            identifier,
            arguments,
            variadic,
        }
    }
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
                        BlockItem::Statement(statement) => {
                            ret += statement.required_stack();
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

    pub fn into_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            return_type: self.return_type.clone(),
            identifier: self.identifier.clone(),
            arguments: self.arguments.clone(),
            variadic: false,
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

impl BlockItem {
    fn required_stack(&self) -> usize {
        match self {
            BlockItem::Statement(statement) => statement.required_stack(),
            BlockItem::Declaration(decl) => decl.type_specifier.size(),
        }
    }
}

// #[derive(Debug, Clone)]
// pub enum Declaration {
//     VariableDeclaration(TypeSpecifier, Identifier),
//     VariableDeclarationAssignment(TypeSpecifier, Identifier, Box<Expression>),
// }

#[derive(Debug, Clone)]
pub enum If {
    SingleBranch(Box<Expression>, Box<Statement>), // if (expr) {statement}
    TwoBranch(Box<Expression>, Box<Statement>, Box<Statement>), // if (expr) {statement0} else {statement1}
}

#[derive(Debug, Clone)]
pub enum Statement {
    ReturnExpression(Box<Expression>),
    Expression(Box<Expression>),
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
pub enum LValue {
    Identifier(Identifier),
    PointerDereference(Identifier, Box<Expression>), // Also used for arrayElement
    PointerDereferenceConstant(Identifier, usize),
}

impl Statement {
    fn required_stack(&self) -> usize {
        match self {
            Statement::ReturnExpression(_) => 0,
            Statement::Expression(_) => 0,
            Statement::If(if_statement) => match if_statement {
                If::SingleBranch(_, statement) => statement.required_stack(),
                If::TwoBranch(_, st_a, st_b) => st_a.required_stack() + st_b.required_stack(),
            },
            Statement::CompoundStatement(statement) => statement.required_stack(),
            Statement::For(_, _, _, statement) => statement.required_stack(),
            Statement::ForDecl(decl, _, _, statement) => {
                decl.type_specifier.size() + statement.required_stack()
            }
            Statement::While(_, statement) => statement.required_stack(),
            Statement::Do(statement, _) => statement.required_stack(),
            Statement::Break => 0,
            Statement::Continue => 0,
            Statement::Empty => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompoundStatement {
    Empty,
    StatementList(Vec<BlockItem>),
}

impl CompoundStatement {
    fn required_stack(&self) -> usize {
        match self {
            CompoundStatement::Empty => 0,
            CompoundStatement::StatementList(list) => list.iter().map(|e| e.required_stack()).sum(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    LiteralNum(i64),
    LiteralString(String),
    UnaryOp(UnaryOp, Box<Expression>),
    Op(Box<Expression>, BinaryOperator, Box<Expression>),
    Variable(Identifier),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Assignment(LValue, Box<Expression>),
    CompoundAssignment(Identifier, BinaryOperator, Box<Expression>),
    FunctionCall(Identifier, Vec<Box<Expression>>), // name+arguments
    IndexOperator(Identifier, Box<Expression>), // name + index expr
    Ampersand(Identifier),
    PostPlusPlus(Identifier),
    PostMinusMinus(Identifier),
    PrePlusPlus(Identifier),
    PreMinusMinus(Identifier),
    SizeOf(Box<Expression>),
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

#[derive(Debug, Clone, PartialEq)]
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
    Pointer(Box<TypeSpecifier>),
    Array(Box<TypeSpecifier>, usize),
}

impl TypeSpecifier {
    pub fn size(&self) -> usize {
        // Technically compliant with standard: https://en.cppreference.com/w/c/language/arithmetic_types
        // "At least 8 bytes"
        match self {
            TypeSpecifier::Char => 1,
            TypeSpecifier::Short => 8,
            TypeSpecifier::Int => 8, // TODO: fix this
            TypeSpecifier::Long => 8,
            TypeSpecifier::Signed => 8,
            TypeSpecifier::Unsigned => 8,
            TypeSpecifier::Float => todo!(),
            TypeSpecifier::Double => todo!(),
            TypeSpecifier::Void => todo!(),
            TypeSpecifier::Pointer(_) => 8, // We are on 64bit machine
            TypeSpecifier::Array(t, count) => t.size() * count, 
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
}
