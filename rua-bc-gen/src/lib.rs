use std::collections::HashMap;

use rua_bc::{FuncId, Instruction, Label, ValueId};

pub fn to_bytecode(chunk: &[ast::Statement]) -> (Vec<Vec<Instruction>>, Vec<Instruction>) {
    let mut gen = BcGen {
        funs: vec![],
        instructions: vec![],
        scopes: vec![vec![HashMap::new()]],
        captures: vec![],
        next_value_id: 0,
        next_global_id: 0,
    };

    gen.gen_statements(chunk);

    (gen.funs, gen.instructions)
}

struct BcGen {
    funs: Vec<Vec<Instruction>>,
    instructions: Vec<Instruction>,
    scopes: Vec<Vec<HashMap<String, ValueId>>>,
    captures: Vec<ValueId>,
    next_value_id: usize,
    next_global_id: usize,
}

impl BcGen {
    fn gen_expr(&mut self, expr: &ast::Expr) -> ValueId {
        use ast::Expr::*;

        match expr {
            Nil(_span) => {
                self.instructions.push(Instruction::Nil);
                self.get_next_id()
            }
            Bool(bool) => {
                self.instructions.push(Instruction::Bool(bool.value));
                self.get_next_id()
            }
            Number(number) => {
                self.instructions.push(Instruction::Number(number.value));
                self.get_next_id()
            }
            StringLit(string_lit) => self.string(string_lit.value.clone()),
            Vararg(_span) => {
                unimplemented!()
                // self.instructions.push(Instruction::Vai
            }
            Variable(ast::Variable::Variable(name)) => self.resolve(name).unwrap(),
            Variable(ast::Variable::Index(index)) => {
                let table = self.gen_expr(&index.lhs);
                let field = self.gen_expr(&index.idx);
                self.get_field(table, field)
            }
            Call(call) => self.gen_call(call),
            MethodCall(method) => self.gen_method_call(method),
            Add(add) => {
                let lhs = self.gen_expr(&add.lhs);
                let rhs = self.gen_expr(&add.rhs);
                self.instructions.push(Instruction::Add(lhs, rhs));
                self.get_next_id()
            }
            Sub(sub) => {
                let lhs = self.gen_expr(&sub.lhs);
                let rhs = self.gen_expr(&sub.rhs);
                self.instructions.push(Instruction::Sub(lhs, rhs));
                self.get_next_id()
            }
            Mul(mul) => {
                let lhs = self.gen_expr(&mul.lhs);
                let rhs = self.gen_expr(&mul.rhs);
                self.instructions.push(Instruction::Mul(lhs, rhs));
                self.get_next_id()
            }
            Div(div) => {
                let lhs = self.gen_expr(&div.lhs);
                let rhs = self.gen_expr(&div.rhs);
                self.instructions.push(Instruction::Div(lhs, rhs));
                self.get_next_id()
            }
            Pow(pow) => {
                let lhs = self.gen_expr(&pow.lhs);
                let rhs = self.gen_expr(&pow.rhs);
                self.instructions.push(Instruction::Pow(lhs, rhs));
                self.get_next_id()
            }
            Neg(neg) => {
                let rhs = self.gen_expr(&neg.rhs);
                self.instructions.push(Instruction::Neg(rhs));
                self.get_next_id()
            }
            Eq(eq) => {
                let lhs = self.gen_expr(&eq.lhs);
                let rhs = self.gen_expr(&eq.rhs);
                self.instructions.push(Instruction::Eq(lhs, rhs));
                self.get_next_id()
            }
            NotEq(not_eq) => {
                let lhs = self.gen_expr(&not_eq.lhs);
                let rhs = self.gen_expr(&not_eq.rhs);
                self.instructions.push(Instruction::NotEq(lhs, rhs));
                self.get_next_id()
            }
            Greater(greater) => {
                let lhs = self.gen_expr(&greater.lhs);
                let rhs = self.gen_expr(&greater.rhs);
                self.instructions.push(Instruction::Greater(lhs, rhs));
                self.get_next_id()
            }
            Less(less) => {
                let lhs = self.gen_expr(&less.lhs);
                let rhs = self.gen_expr(&less.rhs);
                self.instructions.push(Instruction::Less(lhs, rhs));
                self.get_next_id()
            }
            GreaterEq(greater_eq) => {
                let lhs = self.gen_expr(&greater_eq.lhs);
                let rhs = self.gen_expr(&greater_eq.rhs);
                self.instructions.push(Instruction::GreaterEq(lhs, rhs));
                self.get_next_id()
            }
            LessEq(less_eq) => {
                let lhs = self.gen_expr(&less_eq.lhs);
                let rhs = self.gen_expr(&less_eq.rhs);
                self.instructions.push(Instruction::LessEq(lhs, rhs));
                self.get_next_id()
            }
            And(and) => {
                let lhs = self.gen_expr(&and.lhs);
                let rhs = self.gen_expr(&and.rhs);
                self.instructions.push(Instruction::And(lhs, rhs));
                self.get_next_id()
            }
            Or(or) => {
                let lhs = self.gen_expr(&or.lhs);
                let rhs = self.gen_expr(&or.rhs);
                self.instructions.push(Instruction::Or(lhs, rhs));
                self.get_next_id()
            }
            Not(not) => {
                let rhs = self.gen_expr(&not.rhs);
                self.instructions.push(Instruction::Not(rhs));
                self.get_next_id()
            }
            Concat(concat) => {
                let lhs = self.gen_expr(&concat.lhs);
                let rhs = self.gen_expr(&concat.rhs);
                self.instructions.push(Instruction::Concat(lhs, rhs));
                self.get_next_id()
            }
            Len(len) => {
                let rhs = self.gen_expr(&len.rhs);
                self.instructions.push(Instruction::Len(rhs));
                self.get_next_id()
            }
            Table(table) => {
                let mut i = 0;
                for field in &table.fields {
                    let name = if let Some(field_name) = &field.name {
                        self.gen_expr(field_name)
                    } else {
                        self.instructions.push(Instruction::Number(i as f64));
                        i += 1;
                        self.get_next_id()
                    };
                    let value = self.gen_expr(&field.value);
                    self.instructions
                        .push(Instruction::TablePrepare(name, value));
                }
                self.instructions.push(Instruction::Table);
                self.get_next_id()
            }
            Function(func) => {
                let mut old_instructions = std::mem::replace(&mut self.instructions, vec![]);
                let old_captures = std::mem::replace(&mut self.captures, vec![]);
                let old_next_value_id = std::mem::replace(&mut self.next_value_id, 0);
                self.scopes.push(vec![HashMap::new()]);

                {
                    for arg in &func.params {
                        let val = self.get_next_id();
                        self.define_local(arg, val);
                    }

                    self.gen_statements(&func.body);

                    for capture in &self.captures {
                        old_instructions.push(Instruction::PrepareCapture(*capture));
                    }

                    old_instructions.push(Instruction::Function(FuncId(self.funs.len())));
                }

                let fun_instructions = std::mem::replace(&mut self.instructions, old_instructions);
                self.captures = old_captures;
                self.next_value_id = old_next_value_id;
                self.scopes.pop();

                self.funs.push(fun_instructions);
                self.get_next_id()
            }
        }
    }

    fn gen_statements(&mut self, statements: &[ast::Statement]) {
        for statement in statements {
            self.gen_statement(statement);
        }
    }

    fn gen_statement(&mut self, statement: &ast::Statement) {
        use ast::Statement::*;

        match statement {
            Assign(assign) => {
                for expr in &assign.rhs {
                    let value = self.gen_expr(expr);
                    self.instructions.push(Instruction::PrepareAssign(value));
                }

                for var in &assign.lhs {
                    match var {
                        ast::Variable::Variable(name) => {
                            if let Some(var) = self.resolve(name) {
                                self.instructions.push(Instruction::Assign(var));
                            } else {
                                self.define_global(name);
                            }
                        }
                        ast::Variable::Index(index) => {
                            let table = self.gen_expr(&index.lhs);
                            let field = self.gen_expr(&index.idx);

                            self.instructions
                                .push(Instruction::SetField { table, field });
                        }
                    }
                }
            }
            LocalAssign(assign) => {
                for expr in &assign.exprs {
                    let value = self.gen_expr(expr);
                    self.instructions.push(Instruction::PrepareAssign(value));
                }

                assign.vars.iter().for_each(|var| {
                    self.instructions.push(Instruction::Local);
                    let value = self.get_next_id();
                    self.define_local(&var.name, value);
                });
            }
            Block(block) => self.scoped(|this| {
                this.gen_statements(&block);
            }),
            While(while_stmt) => {
                let loop_start = self.get_label_here();
                let cond = self.gen_expr(&while_stmt.condition);
                let branch_inst = self.get_label_here();
                self.instructions.push(Instruction::Nil);
                self.gen_statements(&while_stmt.body);
                self.instructions.push(Instruction::Jump(loop_start));
                let loop_end = self.get_label_here();
                self.instructions[branch_inst.0] = Instruction::Branch(cond, loop_end);
            }
            If(if_stmt) => {
                let mut jumps = Vec::with_capacity(if_stmt.branches.len());

                for branch in &if_stmt.branches {
                    let cond = self.gen_expr(&branch.condition);
                    let branch_inst = self.get_label_here();
                    self.instructions.push(Instruction::Nil);
                    self.gen_statements(&branch.body);
                    jumps.push(self.get_label_here());
                    self.instructions.push(Instruction::Nil);
                    let next_cond = self.get_label_here();
                    self.instructions[branch_inst.0] = Instruction::Branch(cond, next_cond);
                }

                let if_end = self.get_label_here();
                for jump in jumps {
                    self.instructions[jump.0] = Instruction::Jump(if_end);
                }
            }
            Repeat(_repeat) => {
                unimplemented!()
            }
            For(_for_stmt) => {
                unimplemented!()
            }
            NumericalFor(_num_for) => {
                unimplemented!()
            }
            Label(_label) => {
                unimplemented!()
            }
            Goto(_label) => {
                unimplemented!()
            }
            Call(call) => {
                self.gen_call(call);
            }
            MethodCall(method) => {
                self.gen_method_call(method);
            }
            Break => {
                unimplemented!()
            }
            Return(exprs) => {
                for expr in &exprs.values {
                    let value = self.gen_expr(expr);
                    self.instructions.push(Instruction::PrepareReturn(value));
                }
                self.instructions.push(Instruction::Return);
            }
        }
    }

    fn get_call(&mut self, func: ValueId) -> ValueId {
        self.instructions.push(Instruction::Call(func));
        self.get_next_id()
    }

    fn gen_call(&mut self, call: &ast::Call) -> ValueId {
        let func = self.gen_expr(&call.lhs);
        for arg in &call.args {
            let value = self.gen_expr(arg);
            self.instructions.push(Instruction::CallPrepare(value));
        }
        self.get_call(func)
    }

    fn gen_method_call(&mut self, method: &ast::MethodCall) -> ValueId {
        let this = self.gen_expr(&method.lhs);
        let name = self.string(method.name.value.clone());
        let func = self.get_field(this, name);

        self.instructions.push(Instruction::CallPrepare(this));
        for arg in &method.args {
            let value = self.gen_expr(arg);
            self.instructions.push(Instruction::CallPrepare(value));
        }
        self.get_call(func)
    }

    fn get_next_id(&mut self) -> ValueId {
        let id = self.next_value_id;
        self.next_value_id += 1;
        ValueId::Local(id)
    }

    fn string(&mut self, value: String) -> ValueId {
        self.instructions.push(Instruction::String(value));
        self.get_next_id()
    }

    fn get_field(&mut self, table: ValueId, field: ValueId) -> ValueId {
        let id = self.get_next_id();

        self.instructions
            .push(Instruction::GetField { table, field });

        id
    }

    fn get_label_here(&self) -> Label {
        Label(self.instructions.len())
    }

    fn scoped(&mut self, fun: impl FnOnce(&mut Self)) {
        self.scopes.last_mut().unwrap().push(HashMap::new());
        let next_id = self.next_value_id;
        fun(self);
        self.instructions.push(Instruction::Drop(next_id));
        self.next_value_id = next_id;
        self.scopes.last_mut().unwrap().pop();
    }

    fn resolve(&mut self, name: &ast::Ident) -> Option<ValueId> {
        self.scopes
            .last()
            .unwrap()
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name.value).copied())
            .or_else(|| {
                self.scopes.iter().rev().skip(1).find_map(|scopes| {
                    scopes.iter().rev().find_map(|scope| {
                        scope.get(&name.value).copied().map(|value| {
                            let id = self.captures.len();
                            self.captures.push(value);
                            ValueId::Capture(id)
                        })
                    })
                })
            })
    }

    fn define_local(&mut self, name: &ast::Ident, value: ValueId) {
        if self
            .scopes
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .insert(name.value.clone(), value)
            .is_some()
        {
            ()
        } else {
            ()
        }
    }

    fn define_global(&mut self, name: &ast::Ident) {
        let id = self.next_global_id;
        self.next_global_id += 1;
        let global = ValueId::Global(id);

        self.instructions.push(Instruction::Assign(global));
        if self.scopes[0][0]
            .insert(name.value.clone(), global)
            .is_some()
        {
            ()
        } else {
            ()
        }
    }
}
