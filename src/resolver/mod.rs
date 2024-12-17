mod scoped_var_map;

use crate::lexer::token::Token;
use crate::parser::ast::*;
use scoped_var_map::ScopedVarMap;
use thiserror::Error;

#[derive(Debug)]
pub struct ResolveError {
    pub token: Token,
    pub error_type: ResolverErrorType,
}

#[derive(Error, Debug)]
pub enum ResolverErrorType {
    #[error("Duplicate variable in scope (previous declaration on {prev_token:?}")]
    DuplicateVariable { prev_token: Token },

    #[error("Undeclared variable")]
    UndeclaredVariable,

    #[error("Invalid lvalue for assignment")]
    InvalidLValue,
}

pub type ScopeDepth = i32;
pub type MapEntry = WithToken<String>;

pub struct Resolver {
    scope: ScopedVarMap,
    scope_depth: i32,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scope: ScopedVarMap::new(),
            scope_depth: 0,
        }
    }

    fn make_unique(&self, name: &String) -> String {
        format!("{}.{}", name, self.scope_depth)
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

    fn visit_conditional(&mut self, expr: Conditional) -> ResolveResult<Expr> {
        Ok(Expr::Conditional(Conditional {
            cond: Box::new(self.visit_expr(*expr.cond)?),
            then_expr: Box::new(self.visit_expr(*expr.then_expr)?),
            else_expr: Box::new(self.visit_expr(*expr.else_expr)?),
            qmark: expr.qmark,
            colon: expr.colon,
        }))
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
        if let Some(val) = self.scope.lookup(&**name) {
            Ok(Expr::Var(val.clone()))
        } else {
            Err(ResolveError {
                token: name.1,
                error_type: ResolverErrorType::UndeclaredVariable,
            })
        }
    }
}

impl StmtVisitor<ResolveResult<Stmt>> for Resolver {
    fn visit_compound(&mut self, block: Block) -> ResolveResult<Stmt> {
        self.scope.insert_scope();
        self.scope_depth += 1;

        let block = Block(
            block
                .0
                .into_iter()
                .map(|b| self.visit_block_item(b))
                .collect::<ResolveResult<_>>()?,
        );

        self.scope_depth -= 1;
        self.scope.unwind();
        Ok(Stmt::Compound(block))
    }

    fn visit_expression(&mut self, expr: Expr) -> ResolveResult<Stmt> {
        Ok(Stmt::Expression(self.visit_expr(expr)?))
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
}

impl ASTVisitor for Resolver {
    type BlockItemResult = ResolveResult<BlockItem>;
    type ExprResult = ResolveResult<Expr>;
    type FuncDefResult = ResolveResult<FunctionDef>;
    type ProgramResult = ResolveResult<Program>;
    type StmtResult = ResolveResult<Stmt>;
    type VarDeclResult = ResolveResult<VarDecl>;

    fn visit_block_item(&mut self, block_item: BlockItem) -> Self::BlockItemResult {
        Ok(match block_item {
            BlockItem::Stmt(stmt) => BlockItem::Stmt(self.visit_stmt(stmt)?),
            BlockItem::VarDecl(decl) => BlockItem::VarDecl(self.visit_var_decl(decl)?),
        })
    }

    fn visit_function_def(&mut self, function_def: FunctionDef) -> Self::FuncDefResult {
        let Stmt::Compound(body) = self.visit_compound(function_def.body)? else {
            unreachable!();
        };
        Ok(FunctionDef {
            name: function_def.name,
            body,
        })
    }
    fn visit_program(&mut self, program: Program) -> Self::ProgramResult {
        let body = program
            .0
            .into_iter()
            .map(|f| self.visit_function_def(f))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program(body))
    }

    fn visit_var_decl(&mut self, var_decl: VarDecl) -> Self::VarDeclResult {
        println!(
            "scope stack: {:#?}, scope depth: {}\n",
            self.scope, self.scope_depth
        );
        match self.scope.get(&*var_decl.name) {
            Some(WithToken(_, tok)) => {
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

                self.scope.insert(old_name.0, new_name.clone());

                let new_init = var_decl
                    .init
                    .map(|expr| self.visit_expr(expr))
                    .transpose()?;
                Ok(VarDecl {
                    name: new_name,
                    init: new_init,
                })
            }
        }
    }
}
