mod scoped_var_map;

use crate::lexer::token::Token;
use crate::parser::ast::{self, *};
use crate::type_checker::r#type::Type;
use scoped_var_map::ScopedVarMap;
use thiserror::Error;

#[derive(Error, Debug)]
#[error("ResolveError at {token:?}: {error_type}")]
pub struct ResolveError {
    pub token: Token,
    #[source]
    pub error_type: ResolverErrorType,
}

#[derive(Error, Debug)]
pub enum ResolverErrorType {
    #[error("'break' used outside a loop")]
    BreakOutsideLoop,

    #[error("'continue' used outside a loop")]
    ContinueOutsideLoop,

    #[error("Duplicate variable in scope (previous declaration on {prev_token:?}")]
    DuplicateVariable { prev_token: Token },

    #[error("Invalid lvalue for assignment")]
    InvalidLValue,

    #[error("symbol redeclared as different kind of value (previously declared {prev_token:?} of kind `{prev_kind}`, now declared as `{new_kind}`)")]
    RedeclaredAsDifferentKind {
        prev_token: Token,
        prev_kind: &'static str,
        new_kind: &'static str,
    },

    #[error("Undeclared variable")]
    UndeclaredVariable,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Linkage {
    External,
    Internal,
    None,
}

#[derive(Debug)]
pub struct MapEntry {
    pub name: WithToken<String>,
    pub linkage: Linkage,
}

pub type ScopeDepth = i32;

pub struct Resolver {
    scope: ScopedVarMap,
    scope_depth: i32,
    loop_count: i32,
    switch_count: i32,
    loop_id_stack: Vec<i32>,
    switch_id_stack: Vec<i32>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scope: ScopedVarMap::new(),
            scope_depth: 0,
            loop_count: 0,
            loop_id_stack: Vec::new(),
            switch_count: 0,
            switch_id_stack: Vec::new(),
        }
    }

    fn begin_scope(&mut self) {
        self.scope.insert_scope();
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope.unwind();
        self.scope_depth -= 1;
    }

    fn make_unique(&self, name: &String) -> String {
        format!("{}.{}", name, self.scope_depth)
    }

    fn new_loop(&mut self) -> i32 {
        self.loop_count += 1;
        self.loop_id_stack.push(self.loop_count);
        self.loop_count
    }

    fn new_switch(&mut self) -> i32 {
        self.switch_count += 1;
        self.switch_id_stack.push(self.switch_count);
        self.switch_count
    }
}

pub type ResolveResult<T> = Result<T, ResolveError>;

impl ExprVisitor<ResolveResult<Expr>> for Resolver {
    fn visit_assign(&mut self, expr: Assign) -> ResolveResult<Expr> {
        if let Expr::Var(_) = *expr.lhs {
            Ok(Expr::Assign(Assign {
                eq_sign: expr.eq_sign,
                lhs: Box::new(self.visit_expr(*expr.lhs)?),
                rhs: Box::new(self.visit_expr(*expr.rhs)?),
            }))
        } else {
            Err(ResolveError {
                token: expr.eq_sign,
                error_type: ResolverErrorType::InvalidLValue,
            })
        }
    }

    fn visit_binary(&mut self, expr: Binary) -> ResolveResult<Expr> {
        Ok(Expr::Binary(Binary {
            op: expr.op,
            lhs: Box::new(self.visit_expr(*expr.lhs)?),
            rhs: Box::new(self.visit_expr(*expr.rhs)?),
        }))
    }

    fn visit_comma(&mut self, expr: Comma) -> ResolveResult<Expr> {
        Ok(Expr::Comma(Comma(
            Box::new(self.visit_expr(*expr.0)?),
            Box::new(self.visit_expr(*expr.1)?),
        )))
    }

    fn visit_conditional(&mut self, expr: Conditional) -> ResolveResult<Expr> {
        Ok(Expr::Conditional(Conditional {
            cond: Box::new(self.visit_expr(*expr.cond)?),
            then_expr: Box::new(self.visit_expr(*expr.then_expr)?),
            else_expr: Box::new(self.visit_expr(*expr.else_expr)?),
            qmark: expr.qmark,
            colon: expr.colon,
        }))
    }

    fn visit_function_call(&mut self, call: FunctionCall) -> ResolveResult<Expr> {
        let Expr::Var(name) = *call.name else {
            unreachable!()
        };
        if let Some(entry) = self.scope.lookup(&*name) {
            Ok(Expr::FunctionCall(FunctionCall {
                name: Box::new(Expr::Var(entry.name.clone())),
                args: call
                    .args
                    .into_iter()
                    .map(|arg| arg.map(|e| self.visit_expr(e)).transpose())
                    .collect::<Result<Vec<_>, _>>()?,
            }))
        } else {
            Err(ResolveError {
                token: name.1,
                error_type: ResolverErrorType::UndeclaredVariable,
            })
        }
    }

    fn visit_literal(&mut self, lit: WithToken<Literal>) -> ResolveResult<Expr> {
        Ok(Expr::Literal(lit))
    }

    fn visit_unary(&mut self, expr: Unary) -> ResolveResult<Expr> {
        Ok(Expr::Unary(Unary {
            op: expr.op,
            expr: Box::new(self.visit_expr(*expr.expr)?),
            postfix: expr.postfix,
        }))
    }

    fn visit_var(&mut self, name: WithToken<String>) -> ResolveResult<Expr> {
        if let Some(entry) = self.scope.lookup(&**name) {
            Ok(Expr::Var(entry.name.clone()))
        } else {
            Err(ResolveError {
                token: name.1,
                error_type: ResolverErrorType::UndeclaredVariable,
            })
        }
    }
}

impl StmtVisitor<ResolveResult<Stmt>> for Resolver {
    fn visit_break(&mut self, break_stmt: BreakStmt) -> ResolveResult<Stmt> {
        match self.loop_id_stack.last() {
            Some(loop_id) => Ok(Stmt::Break(BreakStmt {
                loop_or_switch: true,
                id: break_stmt.id.map(|_| *loop_id),
            })),
            _ => match self.switch_id_stack.last() {
                Some(switch_id) => Ok(Stmt::Break(BreakStmt {
                    loop_or_switch: false,
                    id: break_stmt.id.map(|_| *switch_id),
                })),
                _ => Err(ResolveError {
                    token: break_stmt.id.1,
                    error_type: ResolverErrorType::BreakOutsideLoop,
                }),
            },
        }
    }

    fn visit_continue(&mut self, continue_stmt: WithToken<i32>) -> ResolveResult<Stmt> {
        match self.loop_id_stack.last() {
            Some(loop_id) => Ok(Stmt::Continue(WithToken(*loop_id, continue_stmt.1))),
            _ => Err(ResolveError {
                token: continue_stmt.1,
                error_type: ResolverErrorType::ContinueOutsideLoop,
            }),
        }
    }

    fn visit_compound(&mut self, stmt: CompoundStmt) -> ResolveResult<Stmt> {
        if stmt.introduce_scope {
            self.begin_scope();
        }

        let block = Block(
            stmt.block
                .0
                .into_iter()
                .map(|b| self.visit_block_item(b))
                .collect::<ResolveResult<_>>()?,
        );

        if stmt.introduce_scope {
            self.end_scope();
        }
        Ok(Stmt::Compound(CompoundStmt {
            introduce_scope: stmt.introduce_scope,
            block,
        }))
    }

    fn visit_expression(&mut self, expr: Expr) -> ResolveResult<Stmt> {
        Ok(Stmt::Expression(self.visit_expr(expr)?))
    }

    // loop id in loops

    fn visit_for(&mut self, for_stmt: ForStmt) -> ResolveResult<Stmt> {
        self.scope.insert_scope();
        self.scope_depth += 1;
        let loop_id = self.new_loop();

        let init_token = for_stmt.initializer.1;
        let initializer = for_stmt
            .initializer
            .0
            .map(|init| match init {
                ForStmtInitializer::Expr(expr) => {
                    Ok(ForStmtInitializer::Expr(self.visit_expr(expr)?))
                }
                ForStmtInitializer::VarDecl(decls) => Ok(ForStmtInitializer::VarDecl(
                    decls
                        .into_iter()
                        .map(|decl| self.visit_var_decl(decl))
                        .collect::<Result<Vec<_>, _>>()?,
                )),
            })
            .transpose()?;

        let condition = for_stmt
            .condition
            .map_inner(|c| self.visit_expr(c))
            .raise_result()?;
        let step = for_stmt
            .step
            .map_inner(|s| self.visit_expr(s))
            .raise_result()?;
        let body = Box::new(self.visit_stmt(*for_stmt.body)?);

        let resolved_for = ForStmt {
            loop_id,
            initializer: WithToken(initializer, init_token),
            condition,
            step,
            body,
        };

        self.loop_id_stack.pop();
        self.scope.unwind();
        self.scope_depth -= 1;
        Ok(Stmt::For(resolved_for))
    }

    fn visit_goto(&mut self, label: WithToken<String>) -> ResolveResult<Stmt> {
        Ok(Stmt::Goto(label))
    }

    fn visit_if(&mut self, if_stmt: IfStmt) -> ResolveResult<Stmt> {
        Ok(Stmt::If(IfStmt {
            cond: if_stmt.cond.map(|c| self.visit_expr(c)).transpose()?,
            then: Box::new(self.visit_stmt(*if_stmt.then)?),
            else_clause: if_stmt
                .else_clause
                .map(|ec| ec.map(|e| self.visit_stmt(e)).transpose())
                .transpose()?
                .map(Box::new),
        }))
    }

    fn visit_label(&mut self, label: Label) -> ResolveResult<Stmt> {
        Ok(Stmt::Label(Label {
            name: label.name,
            next_stmt: Box::new(self.visit_stmt(*label.next_stmt)?),
        }))
    }

    fn visit_null(&mut self) -> ResolveResult<Stmt> {
        Ok(Stmt::Null)
    }
    fn visit_return(&mut self, ret_value: WithToken<Expr>) -> ResolveResult<Stmt> {
        Ok(Stmt::Return {
            ret_value: WithToken(self.visit_expr(ret_value.0)?, ret_value.1),
        })
    }

    fn visit_switch(&mut self, switch_stmt: SwitchStmt) -> ResolveResult<Stmt> {
        self.begin_scope();

        let switch_id = self.new_switch();
        let SwitchStmt {
            switch_id: _,
            cond,
            cases,
            default,
        } = switch_stmt;

        let cond = cond.map(|c| Ok(self.visit_expr(c)?)).transpose()?;
        let cases = cases
            .into_iter()
            .map(|case| {
                case.map(|case| {
                    let Stmt::Compound(stmt) = self.visit_compound(case.stmt)? else {
                        unreachable!()
                    };
                    Ok(Case {
                        value: case.value,
                        stmt,
                    })
                })
                .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;

        let Stmt::Compound(default) = self.visit_compound(default)? else {
            unreachable!()
        };

        self.switch_id_stack.pop();
        self.end_scope();

        Ok(Stmt::Switch(SwitchStmt {
            switch_id,
            cond,
            cases,
            default,
        }))
    }

    fn visit_while(&mut self, while_stmt: WhileStmt) -> ResolveResult<Stmt> {
        let loop_id = self.new_loop();
        let stmt = Ok(Stmt::While(WhileStmt {
            loop_id,
            cond: while_stmt.cond.map(|c| self.visit_expr(c)).transpose()?,
            body: Box::new(self.visit_stmt(*while_stmt.body)?),
            do_while: while_stmt.do_while,
        }));
        self.loop_id_stack.pop();
        stmt
    }
}

impl ASTVisitor for Resolver {
    type BlockItemResult = ResolveResult<BlockItem>;
    type ExprResult = ResolveResult<Expr>;
    type FuncDeclResult = ResolveResult<FunctionDecl>;
    type ProgramResult = ResolveResult<Program>;
    type StmtResult = ResolveResult<Stmt>;
    type VarDeclResult = ResolveResult<VarDecl>;

    fn visit_block_item(&mut self, block_item: BlockItem) -> Self::BlockItemResult {
        Ok(match block_item {
            BlockItem::Stmt(stmt) => BlockItem::Stmt(self.visit_stmt(stmt)?),
            BlockItem::VarDecl(decls) => BlockItem::VarDecl(
                decls
                    .into_iter()
                    .map(|decl| self.visit_var_decl(decl))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            BlockItem::FunctionDecl(func_decl) => {
                BlockItem::FunctionDecl(self.visit_function_decl(func_decl)?)
            }
        })
    }

    fn visit_function_decl(&mut self, function_decl: FunctionDecl) -> Self::FuncDeclResult {
        if let Some(prev_entry) = self.scope.lookup(&*function_decl.name.0) {
            if prev_entry.linkage == Linkage::None {
                return Err(ResolveError {
                    token: function_decl.name.1,
                    error_type: ResolverErrorType::RedeclaredAsDifferentKind {
                        prev_token: prev_entry.name.1.clone(),
                        prev_kind: "variable",
                        new_kind: "function",
                    },
                });
            }
        }

        self.scope.insert(
            function_decl.name.0.clone(),
            MapEntry {
                name: function_decl.name.clone(),
                linkage: Linkage::Internal,
            },
        );

        self.begin_scope();
        let params = function_decl
            .params
            .into_iter()
            .map(|p| {
                self.visit_var_decl(ast::VarDecl {
                    name: p,
                    init: None,
                    ty: Type::Int,
                })
                .map(|decl| decl.name)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let body = function_decl
            .body
            .map(|stmt| {
                self.visit_compound(CompoundStmt {
                    introduce_scope: false,
                    ..stmt
                })
            })
            .transpose()?;
        let body = body.map(|stmt| {
            let Stmt::Compound(cs) = stmt else {
                unreachable!()
            };
            cs
        });

        self.end_scope();
        Ok(FunctionDecl {
            name: function_decl.name,
            params,
            body,
        })
    }

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult {
        let body = program
            .0
            .into_iter()
            .map(|f| self.visit_function_decl(f))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program(body))
    }

    fn visit_var_decl(&mut self, var_decl: VarDecl) -> Self::VarDeclResult {
        match self.scope.get(&*var_decl.name) {
            Some(MapEntry {
                name: WithToken(_, tok),
                linkage,
            }) => {
                if *linkage != Linkage::None {
                    return Err(ResolveError {
                        token: tok.clone(),
                        error_type: ResolverErrorType::RedeclaredAsDifferentKind {
                            prev_token: tok.clone(),
                            prev_kind: "function",
                            new_kind: "variable",
                        },
                    });
                }

                let prev_token = tok.clone();
                let token = var_decl.name.1;
                Err(ResolveError {
                    token,
                    error_type: ResolverErrorType::DuplicateVariable { prev_token },
                })
            }
            _ => {
                let old_name = var_decl.name;
                let new_name = WithToken(self.make_unique(&old_name.0), old_name.1);

                self.scope.insert(
                    old_name.0,
                    MapEntry {
                        name: new_name.clone(),
                        linkage: Linkage::None,
                    },
                );

                let new_init = var_decl
                    .init
                    .map(|expr| self.visit_expr(expr))
                    .transpose()?;
                Ok(VarDecl {
                    name: new_name,
                    init: new_init,
                    ty: var_decl.ty,
                })
            }
        }
    }
}
