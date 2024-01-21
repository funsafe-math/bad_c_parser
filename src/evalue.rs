use std::{collections::BTreeSet, collections::HashMap, env::var, ops::Add};

use crate::ast::*;

#[derive(Debug, Default)]
pub struct Context {
    functions: HashMap<String, FunctionDeclaration>,
    defined_functions: BTreeSet<String>,
    asm: Vec<String>,
    stack: Vec<StackFrme>,
    strings: Vec<(String, String)>,
    last_string_ix: usize,
}

impl Context {
    pub fn top_frame(&mut self) -> &mut StackFrme {
        self.stack.last_mut().unwrap()
    }

    pub fn top_frame_const(&self) -> &StackFrme {
        self.stack.last().unwrap()
    }

    pub fn generate_epilogue(&mut self) {
        // TODO: remove variables from stack?

        // self.asm.push(format!("mov rsp,rbp  ; Epilogue"));
        // self.asm.push(format!("pop rbp  ; Epilogue"));
        let size_on_stack = self.top_frame().size_on_stack;
        self.asm.push(format!("add rsp, {}", size_on_stack));
    }

    // Load variable at name to rax
    pub fn load_variable(&mut self, name: &str) -> Result<Variable, String> {
        let offset = self.top_frame().pushed_on_stack_since_begin;
        let variable = self.top_frame_const().get_variable(name)?;

        let ix = variable.position_on_stack;
        let size = variable.type_specifier.size();
        self.asm.push(format!(
            "mov rax, [rsp+{}]  ; Load {}",
            ix + size + offset,
            name
        ));
        Ok(variable)
    }

    pub fn load_ptr_to_variable(&mut self, name: &str) -> Result<Variable, String> {
        let offset = self.top_frame().pushed_on_stack_since_begin;
        let variable = self.top_frame().get_variable(name)?;
        let ix = variable.position_on_stack;
        let size = variable.type_specifier.size();
        self.asm.push(format!("mov rax, rsp  ; Load {} ptr", name));
        self.asm.push(format!(
            "add rax, {}  ; Load {} ptr",
            ix + size + offset,
            name
        ));

        Ok(variable.to_pointer())
    }

    // pub fn load_ptr

    // Save rax value to location of variable name
    pub fn save_variable(&mut self, name: &str) {
        let offset = self.top_frame().pushed_on_stack_since_begin;
        if let Ok(variable) = self.top_frame().get_variable(name) {
            let ix = variable.position_on_stack;
            let size = variable.type_specifier.size();
            self.asm.push(format!(
                "mov [rsp+{}], rax  ; Save {}",
                ix + size + offset,
                name
            ));
        } else {
            println!("Use of undeclared variable {}, cannot save", name);
            // self.asm.push(format!("movq $0, %rax"));
        }
    }

    pub fn emit_push(&mut self, register: &str, comment: &str) {
        self.asm.push(format!("push {}  ; {}", register, comment));
        self.top_frame().pushed_on_stack_since_begin += 8;
    }

    pub fn emit_pop(&mut self, register: &str, comment: &str) {
        self.asm.push(format!("pop {}  ; {}", register, comment));
        assert!(self.top_frame().pushed_on_stack_since_begin >= 8);
        self.top_frame().pushed_on_stack_since_begin -= 8;
    }

    pub fn create_string(&mut self, value: &str) -> String {
        self.last_string_ix += 1;
        let str_name = format!("L.str.{}", self.last_string_ix);
        self.strings.push((str_name.clone(), value.to_string()));
        str_name
    }

    pub fn bytes_to_stack_alignment(&self) -> usize {
        // Stack needs to be 16 bytes aligned
        let top_frame = self.top_frame_const();
        let current_size_on_stack = top_frame.size_on_stack + top_frame.pushed_on_stack_since_begin;
        let alignment = current_size_on_stack & 0xf;
        8 - alignment
    }

    pub fn get_variable_from_identifier(
        &self,
        identifier: &Identifier,
    ) -> Result<Variable, String> {
        self.top_frame_const().get_variable(&identifier.name)
    }
}

pub fn calling_convention() -> [&'static str; 6] {
    ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
}

#[derive(Debug, Clone)]
pub struct Variable {
    type_specifier: TypeSpecifier,
    position_on_stack: usize,
}

impl Variable {
    fn new(type_specifier: TypeSpecifier, position_on_stack: usize) -> Self {
        Self {
            type_specifier,
            position_on_stack,
        }
    }

    fn to_pointer(&self) -> Self {
        Self {
            type_specifier: TypeSpecifier::Pointer(Box::new(self.type_specifier.clone())),
            position_on_stack: self.position_on_stack,
        }
    }

    fn is_pointer(&self) -> bool {
        match self.type_specifier {
            TypeSpecifier::Pointer(_) => true,
            TypeSpecifier::Array(_, _) => true,
            _ => false,
        }
    }

    fn dereference_type(&self) -> Result<TypeSpecifier, String> {
        match &self.type_specifier {
            TypeSpecifier::Pointer(t) => Ok(*t.clone()),
            TypeSpecifier::Array(t, _) => Ok(*t.clone()),
            _ => Err("Type is not deferencable".to_string()),
        }
    }
}

#[derive(Debug, Default)]
pub struct StackFrme {
    pub variables: HashMap<String, Variable>,
    biggest_ix: usize,
    pub size_on_stack: usize,
    pub last_label: usize,
    pushed_on_stack_since_begin: usize,
}

impl StackFrme {
    fn add_variable(&mut self, type_specifier: TypeSpecifier, name: &str) -> usize {
        let size_in_bytes = 8;
        let to_add = self.biggest_ix;
        let _ret = self
            .variables
            .insert(name.to_string(), Variable::new(type_specifier, to_add));
        self.biggest_ix += size_in_bytes;
        assert!(self.biggest_ix < self.size_on_stack);
        return to_add;
    }

    fn get_variable_ix(&self, name: &str) -> Option<usize> {
        if let Some(variable) = self.variables.get(name) {
            Some(variable.position_on_stack)
        } else {
            None
        }
    }

    fn get_variable(&self, name: &str) -> Result<Variable, String> {
        if let Some(variable) = self.variables.get(name) {
            Ok(variable.clone())
        } else {
            Err(format!(
                "Variable named {} not defined in current scope",
                name
            ))
        }
    }

    fn get_next_label_ix(&mut self) -> usize {
        self.last_label += 1;
        self.last_label
    }
}

pub trait Compile {
    fn compile(&self, ctx: &mut Context);
}

impl Compile for VariableDeclaration {
    fn compile(&self, ctx: &mut Context) {
        match &self.expression {
            Some(expr) => {
                expr.compile(ctx);
            }
            None => {
                println!(
                    "Note: Declaring variable {} without initial value",
                    &self.identifier.name
                );
            }
        }
        let frame = ctx.stack.last_mut().unwrap();
        if frame.variables.contains_key(&self.identifier.name) {
            println!("Error, variable {} already exists", &self.identifier.name);
        } else {
            ctx.top_frame()
                .add_variable(self.type_specifier.clone(), &self.identifier.name);
            ctx.save_variable(&self.identifier.name);
        }
    }
}

impl Expression {
    pub fn get_type(&self, ctx: &Context) -> Result<TypeSpecifier, String> {
        let get_id_type = |identifier: &Identifier| -> Result<TypeSpecifier, String> {
            Ok(ctx
                .top_frame_const()
                .get_variable(&identifier.name)?
                .type_specifier
                .clone())
        };
        match self {
            Expression::LiteralNum(_) => Ok(TypeSpecifier::Long), // TODO: fix
            Expression::LiteralString(s) => Ok(TypeSpecifier::Array(
                Box::new(TypeSpecifier::Char),
                s.len() + 1,
            )), // +1 for null-terminator
            Expression::UnaryOp(op, expr) => expr.get_type(ctx),
            Expression::Op(lhs, _, rhs) => lhs.get_type(ctx)? + rhs.get_type(ctx)?,
            Expression::Variable(identifier) => get_id_type(identifier),
            Expression::Conditional(_, expr_a, expr_b) => {
                let common_type = expr_a.get_type(ctx)? + expr_b.get_type(ctx)?;
                common_type
            }
            Expression::Assignment(lvalue, expr) => match lvalue {
                LValue::Identifier(i) => get_id_type(i),
                LValue::PointerDereference(_, _) => todo!(),
                LValue::PointerDereferenceConstant(_, _) => todo!(),
            },
            Expression::CompoundAssignment(_, _, _) => todo!(),
            Expression::FunctionCall(_, _) => todo!(),
            Expression::IndexOperator(identifier, _) => {
                let t = get_id_type(identifier)?;
                match t {
                    TypeSpecifier::Pointer(underlying) => Ok(*underlying),
                    TypeSpecifier::Array(underlying, _) => Ok(*underlying),
                    _ => Err(format!(
                        "Variable {} is not a pointer nor array",
                        identifier.name
                    )),
                }
            }
            Expression::Ampersand(identifier) => {
                Ok(TypeSpecifier::Pointer(Box::new(get_id_type(identifier)?)))
            }
            Expression::PostPlusPlus(i) => get_id_type(i),
            Expression::PostMinusMinus(i) => get_id_type(i),
            Expression::PrePlusPlus(i) => get_id_type(i),
            Expression::PreMinusMinus(i) => get_id_type(i),
        }
    }
}

impl Add for TypeSpecifier {
    type Output = Result<TypeSpecifier, String>;

    fn add(self, rhs: TypeSpecifier) -> Self::Output {
        if self == rhs {
            Ok(self.clone())
        } else {
            Err("Not convertable".to_string())
        }
    }
}

impl TypeSpecifier {
    fn convertable_to(&self, other: &Self) -> bool {
        (self.clone() + other.clone()).is_ok()
    }
}

impl Compile for BinaryOperator {
    // Will perform operation of rax and rbx
    fn compile(&self, ctx: &mut Context) {
        match self {
            BinaryOperator::Multiply => {
                ctx.asm.push(format!("imul rax, rbx"));
            }
            BinaryOperator::Divide => {
                // ctx.asm.push(format!("idiv %rbx, %rax"));
                todo!()
            }
            BinaryOperator::Modulo => todo!(),
            BinaryOperator::Add => {
                ctx.asm.push(format!("add rax, rbx"));
            }
            BinaryOperator::Subtract => {
                ctx.asm.push(format!("sub rax, rbx"));
            }
            BinaryOperator::LogicAnd => todo!(),
            BinaryOperator::LogicOr => {
                // HACK: Not compilant with the standard, both values will be evaluated
                ctx.asm.push(format!("add rax, rbx"));
                ctx.asm.push(format!("cmp rax, 0"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setne al"));
            }
            BinaryOperator::LogicEq => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("sete al"));
            }
            BinaryOperator::LogicNeq => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setne al"));
            }
            BinaryOperator::GreaterThen => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("seta al"));
            }
            BinaryOperator::LessThen => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setb al"));
            }
            BinaryOperator::Greq => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setge al"));
            }
            BinaryOperator::Leq => {
                ctx.asm.push(format!("cmp rax, rbx"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setle al"));
            }
        }
    }
}

impl Compile for Expression {
    fn compile(&self, ctx: &mut Context) {
        match &self {
            Expression::LiteralNum(value) => {
                ctx.asm.push(format!("mov rax, {}", value));
            }
            Expression::UnaryOp(op, expr) => {
                expr.compile(ctx);
                match op {
                    UnaryOp::Minus => {
                        ctx.asm.push(format!("neg rax"));
                    }
                    UnaryOp::BitwiseNegation => {
                        ctx.asm.push(format!("not rax"));
                    }
                    UnaryOp::LogicalNegation => {
                        ctx.asm.push(format!("cmp rax, 0  ; if rax==0, set ZF"));
                        ctx.asm.push(format!("mov rax, 0  ; zero eax"));
                        ctx.asm
                            .push(format!("sete al      ; set lower rax to 1 if ZF is set"));
                    }
                }
            }
            Expression::Op(lhs, op, rhs) => {
                lhs.compile(ctx);
                ctx.emit_push("rax", "");
                rhs.compile(ctx);
                ctx.asm.push(format!("mov rbx, rax"));
                ctx.emit_pop("rax", "");
                // lhs - rax
                // rhs - rbx
                op.compile(ctx);
            }
            Expression::Variable(var) => {
                let name = &var.name;
                ctx.load_variable(name);
            }
            Expression::Conditional(conditional, expr_a, expr_b) => {
                let label_ix = ctx.top_frame().get_next_label_ix();
                let else_conditional = format!(".LBBL{}_2", label_ix);
                let post_conditional = format!(".LBBL{}_3", label_ix);
                conditional.compile(ctx);
                ctx.asm.push(format!("cmp rax, 0"));
                ctx.asm.push(format!("je {}", &else_conditional));
                expr_a.compile(ctx);
                ctx.asm.push(format!("jmp {}", &post_conditional));
                ctx.asm.push(format!("{}:", &else_conditional));
                expr_b.compile(ctx);
                ctx.asm.push(format!("{}:", &post_conditional));
            }
            Expression::Assignment(id, expr) => {
                match id {
                    LValue::Identifier(identifier) => {
                        let lhs = ctx.get_variable_from_identifier(identifier).unwrap(); // TODO: replace unwrap with ? operator
                        let rhs_type = expr.get_type(ctx).unwrap();
                        assert!(
                            rhs_type.convertable_to(&lhs.type_specifier),
                            "rhs must be convertable to lhs"
                        );
                        let name = &identifier.name;
                        expr.compile(ctx);
                        ctx.save_variable(name);
                    }
                    LValue::PointerDereference(identifier, index_expr) => {
                        let lhs = ctx.get_variable_from_identifier(identifier).unwrap();
                        assert!(lhs.is_pointer());
                        let loaded_type = ctx.load_variable(&identifier.name).unwrap(); // LOAD
                        ctx.emit_push("rax", "");

                        let dereference_type = loaded_type.dereference_type().unwrap();
                        let register = match dereference_type.size() {
                            1 => "al",
                            2 => "ax",
                            4 => "eax",
                            8 => "rax",
                            _ => panic!(),
                        };

                        index_expr.compile(ctx);
                        ctx.emit_pop("rbx", "");
                        ctx.asm.push(format!(
                            "lea rax, [rbx + rax * {}]",
                            dereference_type.size()
                        ));
                        ctx.emit_push("rax", "");

                        expr.compile(ctx);
                        ctx.emit_pop("rbx", "");
                        ctx.asm.push(format!("mov [rbx], {}", register));
                    }
                    LValue::PointerDereferenceConstant(identifier, ix) => {
                        let lhs = ctx.get_variable_from_identifier(identifier).unwrap();
                        assert!(lhs.is_pointer());
                        let loaded_type = ctx.load_variable(&identifier.name).unwrap(); // LOAD

                        let dereference_type = loaded_type.dereference_type().unwrap();
                        if *ix != 0 {
                            ctx.asm
                                .push(format!("add rax, {}", dereference_type.size() * ix));
                        }
                        ctx.emit_push("rax", "");

                        let register = match dereference_type.size() {
                            1 => "al",
                            2 => "ax",
                            4 => "eax",
                            8 => "rax",
                            _ => panic!(),
                        };

                        expr.compile(ctx);
                        ctx.emit_pop("rbx", "");
                        ctx.asm.push(format!("mov [rbx], {}", register));
                    }
                }
            }
            Expression::CompoundAssignment(id, op, expr) => {
                expr.compile(ctx);
                ctx.asm.push(format!("mov rbx, rax"));
                let name = &id.name;
                ctx.load_variable(name);
                op.compile(ctx);
                ctx.save_variable(name);
            }
            Expression::FunctionCall(id, got_arguments) => {
                if let Some(func) = &ctx.functions.get(&id.name) {
                    let expected_args = &func.arguments.clone();
                    let _variadic = func.variadic;
                    if got_arguments.len() > 6 {
                        println!("Currently, more than 6 function parameters is not supported");
                    } else if !func.variadic && got_arguments.len() != expected_args.len() {
                        println!(
                            "{} Called with invalid number of arguments, expected {}, got {}",
                            &id.name,
                            expected_args.len(),
                            got_arguments.len()
                        );
                    } else {
                        let calling_convention = calling_convention();
                        let n_dirty_registers = got_arguments.len();
                        for reg in calling_convention.iter().take(n_dirty_registers) {
                            ctx.emit_push(reg, "will be param, lets save it");
                        }

                        for (i, (arg, reg)) in
                            got_arguments.iter().zip(calling_convention).enumerate()
                        {
                            arg.compile(ctx);
                            let variable_name = match i < expected_args.len() {
                                true => format!("{}", &expected_args[i].identifier.name),
                                false => format!("varidic-{}", i),
                            };
                            let comment =
                                format!("prepare argument {} of {}", variable_name, id.name);
                            ctx.asm.push(format!("mov {}, rax  ; {}", reg, comment));
                        }

                        let bytes_to_alignment = ctx.bytes_to_stack_alignment();
                        if bytes_to_alignment != 0 {
                            ctx.asm.push(format!(
                                "sub rsp, {}  ; Align the stack for function call",
                                bytes_to_alignment
                            ));
                        }

                        ctx.asm.push(format!("call {}", id.name));

                        if bytes_to_alignment != 0 {
                            ctx.asm.push(format!(
                                "add rsp, {}  ; Restore stack after function call",
                                bytes_to_alignment
                            ));
                        }

                        let n_dirty_registers = got_arguments.len();
                        for reg in calling_convention.iter().take(n_dirty_registers).rev() {
                            ctx.emit_pop(reg, "restore saved");
                        }
                        // ctx.asm.push(format!("add esp, 8"));
                        // ctx.asm.push(format!("mov  esp, ebp"));
                    }
                } else {
                    println!("Function named {} unknown", &id.name);
                }
            }
            Expression::LiteralString(s) => {
                let label = ctx.create_string(&s);
                ctx.asm.push(format!("lea rax, [rel {}]", &label));
                // ctx.asm.push(format!("mov rax, rdi"));
            }
            Expression::IndexOperator(identifier, expr) => {
                //TODO: identifier must be ptr
                let resulting_type = self.get_type(ctx).unwrap(); // TODO: make it '?'
                expr.compile(ctx);
                ctx.asm
                    .push(format!("lea rdx, [rax*{}]", resulting_type.size()));
                ctx.load_variable(&identifier.name);
                ctx.asm.push(format!("add rax, rdx"));
                ctx.asm.push(format!("mov rax, [rax]"));
            }
            Expression::Ampersand(identifier) => {
                ctx.load_ptr_to_variable(&identifier.name);
            }
            Expression::PostPlusPlus(identifier) => {
                ctx.load_variable(&identifier.name);
                ctx.emit_push(
                    "rax",
                    &format!("Save {} pre incrementation", &identifier.name),
                );
                ctx.asm.push(format!("inc rax"));
                ctx.save_variable(&identifier.name);
                ctx.emit_pop("rax", "");
            }
            Expression::PostMinusMinus(identifier) => {
                let name = &identifier.name;
                ctx.load_variable(name);
                ctx.emit_push("rax", &format!("Save {} pre decrementation", name));
                ctx.asm.push(format!("dec rax"));
                ctx.save_variable(name);
                ctx.emit_pop("rax", "");
            }
            Expression::PrePlusPlus(identifier) => {
                let name = &identifier.name;
                ctx.load_variable(name);
                ctx.asm.push(format!("inc rax"));
                ctx.save_variable(name);
            }
            Expression::PreMinusMinus(identifier) => {
                let name = &identifier.name;
                ctx.load_variable(name);
                ctx.asm.push(format!("dec rax"));
                ctx.save_variable(name);
            }
        }
    }
}

impl Compile for Statement {
    fn compile(&self, ctx: &mut Context) {
        match &self {
            Statement::ReturnExpression(return_expr) => {
                return_expr.compile(ctx);
                ctx.generate_epilogue();

                ctx.asm.push(format!("ret"));
            }
            Statement::Expression(expression) => {
                expression.compile(ctx);
            }
            Statement::If(if_statement) => match if_statement {
                If::SingleBranch(expr, statement) => {
                    expr.compile(ctx);
                    let label_ix = ctx.top_frame().get_next_label_ix();
                    let post_conditional = format!(".LBBL{}_post_conditional", label_ix);
                    ctx.asm.push(format!("cmp rax, 0"));
                    ctx.asm.push(format!("je {}", post_conditional));
                    statement.compile(ctx);
                    ctx.asm.push(format!("{}:", post_conditional));
                }
                If::TwoBranch(expr, on_true, on_false) => {
                    expr.compile(ctx);
                    let label_ix = ctx.top_frame().get_next_label_ix();
                    let else_conditional = format!(".LBBL{}_2", label_ix);
                    let post_conditional = format!(".LBBL{}_3", label_ix);
                    ctx.asm.push(format!("cmp rax, 0"));
                    ctx.asm.push(format!("je {}", &else_conditional));
                    on_true.compile(ctx);
                    ctx.asm.push(format!("jmp {}", &post_conditional));
                    ctx.asm.push(format!("{}:", &else_conditional));
                    on_false.compile(ctx);
                    ctx.asm.push(format!("{}:", &post_conditional));
                }
            },
            Statement::CompoundStatement(compound_statement) => match compound_statement {
                CompoundStatement::Empty => {}
                CompoundStatement::StatementList(list) => {
                    for item in list {
                        item.compile(ctx);
                    }
                }
            },
            Statement::For(a_expr, b_expr, c_expr, statement) => {
                let label_ix = ctx.top_frame().get_next_label_ix();
                let post = format!(".LBBL{}_2", label_ix);
                let pre = format!(".LBBL{}_0", label_ix);
                if let Some(a_expr) = a_expr {
                    a_expr.compile(ctx);
                }

                ctx.asm.push(format!("{}:", &pre));
                if let Some(b_expr) = b_expr {
                    b_expr.compile(ctx);
                    ctx.asm.push(format!("cmp rax, 0"));
                    ctx.asm.push(format!("je {}", &post));
                } else {
                    // Infinite loop
                }

                statement.compile(ctx);
                if let Some(c_expr) = c_expr {
                    c_expr.compile(ctx);
                }
                ctx.asm.push(format!("jmp {}", &pre));

                ctx.asm.push(format!("{}:", &post));
            }
            Statement::ForDecl(decl, b_expr, c_expr, statement) => {
                let label_ix = ctx.top_frame().get_next_label_ix();
                let post = format!(".LBBL{}_2", label_ix);
                let pre = format!(".LBBL{}_0", label_ix);
                decl.compile(ctx);

                ctx.asm.push(format!("{}:", &pre));
                if let Some(b_expr) = b_expr {
                    b_expr.compile(ctx);
                    ctx.asm.push(format!("cmp rax, 0"));
                    ctx.asm.push(format!("je {}", &post));
                } else {
                    // Infinite loop
                }

                statement.compile(ctx);
                if let Some(c_expr) = c_expr {
                    c_expr.compile(ctx);
                }
                ctx.asm.push(format!("jmp {}", &pre));

                ctx.asm.push(format!("{}:", &post));
            }
            Statement::While(expr, statement) => {
                let label_ix = ctx.top_frame().get_next_label_ix();
                let pre = format!(".LBBL{}_0", label_ix);
                let post = format!(".LBBL{}_2", label_ix);
                ctx.asm.push(format!("{}:", &pre));
                expr.compile(ctx);
                ctx.asm.push(format!("cmp rax, 0"));
                ctx.asm.push(format!("je {}", &post));
                statement.compile(ctx);
                ctx.asm.push(format!("jmp {}", &pre));
                ctx.asm.push(format!("{}:", &post));
            }
            Statement::Do(statement, expr) => {
                let label_ix = ctx.top_frame().get_next_label_ix();
                let pre = format!(".LBBL{}_0", label_ix);
                let post = format!(".LBBL{}_2", label_ix);

                ctx.asm.push(format!("{}:", &pre));
                statement.compile(ctx);
                expr.compile(ctx);
                ctx.asm.push(format!("cmp rax, 0"));
                ctx.asm.push(format!("je {}", &post));
                ctx.asm.push(format!("jmp {}", &pre));
                ctx.asm.push(format!("{}:", &post));
            }
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Empty => todo!(),
        }
    }
}

impl Compile for BlockItem {
    fn compile(&self, ctx: &mut Context) {
        match self {
            BlockItem::Statement(statement) => {
                statement.compile(ctx);
            }
            BlockItem::Declaration(decl) => {
                decl.compile(ctx);
            }
        }
    }
}

impl Compile for FunctionDefinition {
    fn compile(&self, ctx: &mut Context) {
        ctx.functions
            .insert(self.identifier.name.clone(), self.into_declaration());
        ctx.asm.push(format!("global {}", self.identifier.name));
        ctx.asm.push(format!("{}:", self.identifier.name));
        ctx.defined_functions.insert(self.identifier.name.clone());

        let first_function_instruction_ix = ctx.asm.len();

        // Setup new stack frame
        let frame = StackFrme::default();

        ctx.stack.push(frame);

        ctx.top_frame().size_on_stack = self.required_stack() + 8; // +8 for return ptr

        // Copy arguments onto the stack TODO: fix
        let calling_convention = calling_convention();
        for arg in &self.arguments {
            ctx.top_frame().size_on_stack += arg.type_specifier.size();
            ctx.top_frame()
                .add_variable(arg.type_specifier.clone(), &arg.identifier.name);
        }

        // Allocate space for stack variables
        let required_stack = ctx.top_frame().size_on_stack;
        ctx.asm.push(format!(
            "sub rsp, {}  ; setup stack space for local variables",
            required_stack
        ));
        // ctx.top_frame().size_on_stack = required_stack;

        for (i, arg) in self.arguments.iter().enumerate() {
            ctx.asm.push(format!(
                "mov rax, {}  ; copy argument {} onto stack",
                calling_convention[i], &arg.identifier.name
            ));
            ctx.save_variable(&arg.identifier.name);
        }

        // ctx.asm.push(format!("push rbp; Setup new stack frame"));
        // ctx.asm
        //     .push(format!("mov rbp, rsp ; Setup new stack frame"));

        // Put values into args
        match &self.statement {
            CompoundStatement::Empty => {} // skip
            CompoundStatement::StatementList(list) => {
                for item in list {
                    item.compile(ctx);
                }
            }
        }

        assert!(ctx.stack.len() > 0);

        ctx.stack.pop();
        ctx.asm.push(format!("; just to be sure we return"));

        ctx.asm.push(format!(
            "add rsp, {}  ; deallocate stack space for local variables",
            required_stack
        ));
        ctx.asm.push(format!("ret "));

        // Indent fuction code
        for instr in ctx.asm.iter_mut().skip(first_function_instruction_ix) {
            if instr.starts_with('.') {
                continue;
            }
            *instr = format!("    {}", &instr);
        }
    }
}

impl Program {
    pub fn compile(&self) -> String {
        let mut ctx = Context::default();
        ctx.asm.push(format!("section .text"));
        for item in &self.top_level_items {
            match item {
                TopLevelItem::Function(function) => {
                    println!("Compiling function {:#?}", &function.identifier.name);
                    function.compile(&mut ctx);
                }
                TopLevelItem::VariableDeclaration(_variable) => {
                    todo!()
                }
                TopLevelItem::FunctionDeclaration(function_declaration) => {
                    ctx.functions.insert(
                        function_declaration.identifier.name.clone(),
                        function_declaration.clone(),
                    );
                }
            }
        }

        // Create strings
        ctx.asm.push(format!("section .rodata"));
        for (label, value) in ctx.strings {
            ctx.asm.push(format!("    {} db `{}`, 0", label, value));
        }

        //         ctx.asm.push(r#"
        // print_rax:
        // ; Call printf.
        //     mov esi, eax
        //     ; mov   esi, 0x12345678
        //     lea   rdi, [rel format]
        //     xor   eax, eax
        //     call  printf
        //     ret

        // section .rodata
        //     format db "%d", 10, 0

        // extern printf
        // "#.to_string());
        // ctx.asm.push(format!("extern putchar"));
        // ctx.asm.push(format!("extern printf"));
        // Mark unknown symbols extern
        for (name, _) in &ctx.functions {
            if !ctx.defined_functions.contains(name) {
                ctx.asm.push(format!("extern {}", name));
            }
        }

        ctx.asm.iter().map(|x| format!("{}\n", x)).collect()
    }
}
