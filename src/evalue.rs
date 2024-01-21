use std::{collections::HashMap, fmt::format, hash::Hash, iter::Map, sync::RwLockReadGuard};

use crate::ast::*;

#[derive(Debug, Default)]
struct Context {
    functions: HashMap<String, FunctionDeclaration>,
    asm: Vec<String>,
    stack: Vec<StackFrme>,
}

impl Context {
    pub fn top_frame(&mut self) -> &mut StackFrme {
        self.stack.last_mut().unwrap()
    }

    pub fn generate_epilogue(&mut self) {
        // TODO: remove variables from stack?

        // self.asm.push(format!("mov rsp,rbp  ; Epilogue"));
        // self.asm.push(format!("pop rbp  ; Epilogue"));
        let size_on_stack = self.top_frame().size_on_stack;
        self.asm.push(format!("add rsp, {}", size_on_stack));
    }

    // Load variable at name to rax
    pub fn load_variable(&mut self, name: &str) {
        let offset = self.top_frame().pushed_on_stack_since_begin;
        if let Some(ix) = self.top_frame().get_variable_ix(name) {
            self.asm
                .push(format!("mov rax, [rsp+{}] ; Loading {}", ix + 8 + offset, name));
        } else {
            println!("Use of undeclared variable {}, will load 0", name);
            self.asm.push(format!("mov rax, 0"));
        }
    }

    // Save rax value to location of variable name
    pub fn save_variable(&mut self, name: &str) {
        let offset = self.top_frame().pushed_on_stack_since_begin;
        if let Some(ix) = self.top_frame().get_variable_ix(name) {
            self.asm
                .push(format!("mov [rsp+{}], rax ; saving {}", ix + 8 + offset, name ));
        } else {
            println!("Use of undeclared variable {}, cannot save", name);
            // self.asm.push(format!("movq $0, %rax"));
        }
    }

    pub fn emit_push(&mut self, register: &str, comment: &str){
        self.asm.push(format!("push {}  ; {}", register, comment));
        self.top_frame().pushed_on_stack_since_begin += 8;
    }

    pub fn emit_pop(&mut self, register: &str, comment: &str){
        self.asm.push(format!("pop {}  ; {}", register, comment));
        assert!(self.top_frame().pushed_on_stack_since_begin >= 8);
        self.top_frame().pushed_on_stack_since_begin -= 8;
    }
}

pub fn calling_convention() -> [&'static str; 6] {
    ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
}

#[derive(Debug, Default)]
struct StackFrme {
    pub variables: HashMap<String, usize>,
    biggest_ix: usize,
    pub size_on_stack: usize,
    pub last_label: usize,
    pushed_on_stack_since_begin: usize,
}

impl StackFrme {
    fn add_variable(&mut self, name: &str) -> usize {
        let size_in_bytes = 8;
        let to_add = self.biggest_ix;
        let ret = self.variables.insert(name.to_string(), to_add);
        self.biggest_ix += size_in_bytes;
        return to_add;
    }

    fn get_variable_ix(&self, name: &str) -> Option<usize> {
        return self.variables.get(name).copied();
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
        let mut frame = ctx.stack.last_mut().unwrap();
        if frame.variables.contains_key(&self.identifier.name) {
            println!("Error, variable {} already exists", &self.identifier.name);
        } else {
            ctx.top_frame().add_variable(&self.identifier.name);
            ctx.save_variable(&self.identifier.name);
        }
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
            BinaryOperator::LogicOr => { // HACK: Not compilant with the standard, both values will be evaluated
                ctx.asm.push(format!("add rax, rbx"));
                ctx.asm.push(format!("cmp rax, 0"));
                ctx.asm.push(format!("mov rax, 0"));
                ctx.asm.push(format!("setne al"));
            },
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
            BinaryOperator::GreaterThen => todo!(),
            BinaryOperator::LessThen => todo!(),
            BinaryOperator::Greq => todo!(),
            BinaryOperator::Leq => todo!(),
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
                    UnaryOp::PlusPlus => {
                        ctx.asm.push(format!("add rax, 1"));
                    }
                    UnaryOp::MinusMinus => {
                        ctx.asm.push(format!("sub rax, 1"));
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
            Expression::Conditional(_, _, _) => todo!(),
            Expression::Assignment(id, expr) => {
                expr.compile(ctx);
                let name = &id.name;
                ctx.save_variable(name);
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
                    if expected_args.len() > 6 {
                        println!("Currently, more than 6 function parameters is not supported");
                    } else if got_arguments.len() != expected_args.len() {
                        println!(
                            "{} Called with invalid number of arguments, expected {}, got {}",
                            &id.name,
                            expected_args.len(),
                            got_arguments.len()
                        );
                    } else {
                        let calling_convention = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                        // for (i,arg) in got_arguments.iter().enumerate().rev(){
                        //     arg.compile(ctx);
                        //     ctx.asm.push(format!("push rax  ; prepare argument {} of {}", &expected_args[i].identifier.name, id.name));
                        // }
                        let n_dirty_registers = got_arguments.len();
                        for reg in calling_convention.iter().take(n_dirty_registers) {
                            // ctx.asm
                            //     .push(format!("push {}  ; will be param, lets save it", reg))
                            ctx.emit_push(reg, "will be param, lets save it");
                        }

                        for (i, (arg, reg)) in
                            got_arguments.iter().zip(calling_convention).enumerate()
                        {
                            arg.compile(ctx);
                            ctx.asm.push(format!(
                                "mov {}, rax  ; prepare argument {} of {}",
                                reg, &expected_args[i].identifier.name, id.name
                            ));
                        }
                        ctx.asm.push(format!("call {}", id.name));

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
            Statement::Assignment(_, _) => todo!(),
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
                    let if_conditional = format!(".LBBL{}_2", label_ix);
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
            Statement::ForDecl(_, _, _, _) => todo!(),
            Statement::While(_, _) => todo!(),
            Statement::Do(_, _) => todo!(),
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

        let first_function_instruction_ix = ctx.asm.len();

        let mut required_stack = self.required_stack() + 8; // +8 for return ptr
                                                            // Setup new stack frame
        let mut frame = StackFrme::default();

        ctx.stack.push(frame);

        // Copy arguments onto the stack TODO: fix
        let calling_convention = calling_convention();
        for arg in &self.arguments {
            ctx.top_frame().add_variable(&arg.identifier.name);
            required_stack += arg.type_specifier.size();
        }

        // Allocate space for stack variables
        ctx.asm.push(format!(
            "sub rsp, {}  ; setup stack space for local variables",
            required_stack
        ));
        ctx.top_frame().size_on_stack = required_stack;

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
                TopLevelItem::VariableDeclaration(variable) => {
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
        ctx.asm.push(format!("extern putchar"));

        ctx.asm.iter().map(|x| format!("{}\n", x)).collect()
    }
}
