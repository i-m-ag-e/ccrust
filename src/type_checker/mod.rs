use thiserror::Error;

use crate::{
    lexer::token::Token,
    parser::ast::*,
    parser::ast::{ASTRefVisitor, ExprRefVisitor},
};

pub mod r#type;
use r#type::Type;

pub mod symbol_table;
use symbol_table::{SymbolTable, SymbolTableEntry};

#[derive(Debug, Error)]
#[error("Type error at {token:?}: {kind}")]
pub struct TypeCheckerError {
    pub token: Token,
    #[source]
    pub kind: TypeCheckerErrorKind,
}

#[derive(Debug, Error)]
pub enum TypeCheckerErrorKind {
    #[error("duplicate definitions of function (previously defined at {prev:?})")]
    DuplicateDefFunction { prev: Token },

    #[error("function called with wrong number of arguments; expeected {expected}, got {got}")]
    FunctionCallWrongNumberOfArgs { expected: usize, got: usize },

    #[error(
        "function redeclared with different type\n\tnew declaration hsa type {new}, but previously declared as `{prev}` at {prev_token:?}"
    )]
    FunctionRedeclaredWithDifferentType {
        prev: Type,
        new: Type,
        prev_token: Token,
    },

    #[error("value of type {0} cannot be used as an lvalue")]
    TypeCannotBeLValue(Type),

    #[error("type mismatch: expected `{expected}`, got `{got}`")]
    TypeMismatch { expected: Type, got: Type },
}

pub struct TypeChecker {
    symbol_table: SymbolTable,
}

pub type TypeCheckerResult<T> = Result<T, TypeCheckerError>;

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn add_symbol(&mut self, name: WithToken<String>, ty: Type, defined: bool) {
        self.symbol_table.insert(
            name.0,
            SymbolTableEntry {
                ty,
                token: name.1,
                defined,
            },
        );
    }

    pub fn get_symbol(&self, name: &str) -> Option<&SymbolTableEntry> {
        self.symbol_table.get(name)
    }

    pub fn get_symbol_mut(&mut self, name: &str) -> Option<&mut SymbolTableEntry> {
        self.symbol_table.get_mut(name)
    }

    pub fn get_symbol_type(&self, name: &str) -> Option<&Type> {
        self.get_symbol(name).map(|entry| &entry.ty)
    }
}

impl ExprRefVisitor<TypeCheckerResult<Type>> for TypeChecker {
    fn visit_assign(&mut self, assign: &Assign) -> TypeCheckerResult<Type> {
        let Expr::Var(ref name) = *assign.lhs else {
            unreachable!()
        };

        let rhs_ty = self.visit_expr(&assign.rhs)?;

        if let Some(lhs_ty) = self.get_symbol_type(name) {
            match lhs_ty {
                Type::Function { .. } => Err(TypeCheckerError {
                    token: name.1.clone(),
                    kind: TypeCheckerErrorKind::TypeCannotBeLValue(lhs_ty.clone()),
                }),
                ty if *ty != rhs_ty => Err(TypeCheckerError {
                    token: name.1.clone(),
                    kind: TypeCheckerErrorKind::TypeMismatch {
                        expected: lhs_ty.clone(),
                        got: rhs_ty,
                    },
                }),
                _ => Ok(rhs_ty),
            }
        } else {
            unreachable!("variable must have a type before assigning")
        }
    }

    fn visit_binary(&mut self, expr: &Binary) -> TypeCheckerResult<Type> {
        let lhs_ty = self.visit_expr(&expr.lhs)?;
        let rhs_ty = self.visit_expr(&expr.rhs)?;

        if lhs_ty != rhs_ty {
            return Err(TypeCheckerError {
                token: expr.op.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: lhs_ty,
                    got: rhs_ty,
                },
            });
        }
        Ok(Type::Int)
    }

    fn visit_comma(&mut self, expr: &Comma) -> TypeCheckerResult<Type> {
        let _ = self.visit_expr(&expr.0)?;
        self.visit_expr(&expr.1)
    }

    fn visit_conditional(&mut self, expr: &Conditional) -> TypeCheckerResult<Type> {
        let cond_ty = self.visit_expr(&expr.cond)?;
        let then_ty = self.visit_expr(&expr.then_expr)?;
        let else_ty = self.visit_expr(&expr.else_expr)?;

        if cond_ty != Type::Int {
            return Err(TypeCheckerError {
                token: expr.qmark.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: cond_ty,
                },
            });
        }

        if then_ty != else_ty {
            return Err(TypeCheckerError {
                token: expr.colon.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: then_ty,
                    got: else_ty,
                },
            });
        }

        Ok(then_ty)
    }

    fn visit_function_call(&mut self, call: &FunctionCall) -> TypeCheckerResult<Type> {
        let func_ty = self.visit_expr(&call.name)?;
        let name = match &*call.name {
            Expr::Var(name) => name,
            _ => unreachable!(),
        };

        if let Type::Function { ref params } = func_ty {
            if params.len() != call.args.len() {
                return Err(TypeCheckerError {
                    token: name.1.clone(),
                    kind: TypeCheckerErrorKind::FunctionCallWrongNumberOfArgs {
                        expected: params.len(),
                        got: call.args.len(),
                    },
                });
            }

            for (param, arg) in params.iter().zip(call.args.iter()) {
                let arg_ty = self.visit_expr(arg)?;
                if *param != arg_ty {
                    return Err(TypeCheckerError {
                        token: arg.1.clone(),
                        kind: TypeCheckerErrorKind::TypeMismatch {
                            expected: param.clone(),
                            got: arg_ty,
                        },
                    });
                }
            }

            Ok(Type::Int)
        } else {
            Err(TypeCheckerError {
                token: name.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Function {
                        params: call.args.iter().map(|_| Type::Int).collect(),
                    },
                    got: func_ty,
                },
            })
        }
    }

    fn visit_literal(&mut self, literal: &WithToken<Literal>) -> TypeCheckerResult<Type> {
        match **literal {
            Literal::Integral(_) => Ok(Type::Int),
            Literal::Float(_) => todo!(),
        }
    }

    fn visit_unary(&mut self, expr: &Unary) -> TypeCheckerResult<Type> {
        let ty = self.visit_expr(&expr.expr)?;
        if let Type::Function { .. } = ty {
            Err(TypeCheckerError {
                token: expr.op.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: ty,
                },
            })
        } else {
            Ok(Type::Int)
        }
    }

    fn visit_var(&mut self, name: &WithToken<String>) -> TypeCheckerResult<Type> {
        if let Some(ty) = self.get_symbol_type(&name.0) {
            Ok(ty.clone())
        } else {
            unreachable!("variable must be resolved")
        }
    }
}

impl StmtRefVisitor<TypeCheckerResult<()>> for TypeChecker {
    fn visit_break(&mut self, _: &BreakStmt) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_compound(&mut self, compound_stmt: &CompoundStmt) -> TypeCheckerResult<()> {
        for block_item in &compound_stmt.block.0 {
            self.visit_block_item(block_item)?;
        }
        Ok(())
    }

    fn visit_continue(&mut self, _: &WithToken<i32>) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Expr) -> TypeCheckerResult<()> {
        self.visit_expr(expr)?;
        Ok(())
    }

    fn visit_for(&mut self, for_stmt: &ForStmt) -> TypeCheckerResult<()> {
        match &*for_stmt.initializer {
            Some(ref init) => match init {
                ForStmtInitializer::Expr(e) => {
                    self.visit_expr(e)?;
                }
                ForStmtInitializer::VarDecl(decls) => {
                    decls
                        .iter()
                        .map(|d| self.visit_var_decl(d))
                        .collect::<Result<Vec<_>, _>>()?;
                }
            },
            None => {}
        };

        (*for_stmt.condition)
            .as_ref()
            .map(|c| self.visit_expr(c))
            .transpose()?
            .map(|c| {
                if c != Type::Int {
                    Err(TypeCheckerError {
                        token: for_stmt.condition.1.clone(),
                        kind: TypeCheckerErrorKind::TypeMismatch {
                            expected: Type::Int,
                            got: c,
                        },
                    })
                } else {
                    Ok(())
                }
            })
            .transpose()?;

        (*for_stmt.step)
            .as_ref()
            .map(|s| self.visit_expr(&s))
            .transpose()?;

        self.visit_stmt(&for_stmt.body)?;
        Ok(())
    }

    fn visit_goto(&mut self, _: &WithToken<String>) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_if(&mut self, if_stmt: &IfStmt) -> TypeCheckerResult<()> {
        let cond_ty = self.visit_expr(&if_stmt.cond)?;
        if cond_ty != Type::Int {
            return Err(TypeCheckerError {
                token: if_stmt.cond.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: cond_ty,
                },
            });
        }

        self.visit_stmt(&if_stmt.then)?;
        if let Some(ref else_branch) = if_stmt.else_clause {
            self.visit_stmt(else_branch)?;
        }
        Ok(())
    }

    fn visit_label(&mut self, label: &Label) -> TypeCheckerResult<()> {
        self.visit_stmt(&label.next_stmt)
    }

    fn visit_null(&mut self) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_return(&mut self, ret_value: &WithToken<Expr>) -> TypeCheckerResult<()> {
        let ty = self.visit_expr(&ret_value.0)?;
        if ty != Type::Int {
            return Err(TypeCheckerError {
                token: ret_value.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: ty,
                },
            });
        }
        Ok(())
    }

    fn visit_switch(&mut self, switch_stmt: &SwitchStmt) -> TypeCheckerResult<()> {
        let ty = self.visit_expr(&switch_stmt.cond)?;
        if ty != Type::Int {
            return Err(TypeCheckerError {
                token: switch_stmt.cond.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: ty,
                },
            });
        }

        for case in &switch_stmt.cases {
            self.visit_compound(&case.stmt)?;
        }
        self.visit_compound(&switch_stmt.default)?;

        Ok(())
    }

    fn visit_while(&mut self, while_stmt: &WhileStmt) -> TypeCheckerResult<()> {
        let ty = self.visit_expr(&while_stmt.cond)?;
        if ty != Type::Int {
            return Err(TypeCheckerError {
                token: while_stmt.cond.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: Type::Int,
                    got: ty,
                },
            });
        }

        self.visit_stmt(&while_stmt.body)?;
        Ok(())
    }
}

impl ASTRefVisitor for TypeChecker {
    type BlockItemResult = TypeCheckerResult<()>;
    type ExprResult = TypeCheckerResult<Type>;
    type FuncDeclResult = TypeCheckerResult<()>;
    type ProgramResult = TypeCheckerResult<()>;
    type StmtResult = TypeCheckerResult<()>;
    type VarDeclResult = TypeCheckerResult<()>;

    fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItemResult {
        match block_item {
            BlockItem::FunctionDecl(func_decl) => self.visit_function_decl(func_decl)?,
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt)?,
            BlockItem::VarDecl(var_decl) => {
                for decl in var_decl {
                    self.visit_var_decl(decl)?;
                }
            }
        };

        Ok(())
    }

    fn visit_function_decl(&mut self, function_def: &FunctionDecl) -> Self::FuncDeclResult {
        let func_ty = Type::Function {
            params: function_def.params.iter().map(|_| Type::Int).collect(),
        };

        if let Some(old_decl) = self.get_symbol_mut(&function_def.name.0) {
            if old_decl.ty != func_ty {
                return Err(TypeCheckerError {
                    token: function_def.name.1.clone(),
                    kind: TypeCheckerErrorKind::FunctionRedeclaredWithDifferentType {
                        prev: old_decl.ty.clone(),
                        new: func_ty,
                        prev_token: old_decl.token.clone(),
                    },
                });
            }

            if old_decl.defined && function_def.body.is_some() {
                return Err(TypeCheckerError {
                    token: function_def.name.1.clone(),
                    kind: TypeCheckerErrorKind::DuplicateDefFunction {
                        prev: old_decl.token.clone(),
                    },
                });
            }

            if function_def.body.is_some() {
                old_decl.defined = true;
            }
        } else {
            println!("Declared {:?}", *function_def.name);
            self.add_symbol(
                function_def.name.clone(),
                Type::Function {
                    params: function_def.params.iter().map(|_| Type::Int).collect(),
                },
                function_def.body.is_some(),
            );
        }

        if let Some(ref body) = function_def.body {
            for param in &function_def.params {
                self.add_symbol(param.clone(), Type::Int, true);
            }
            self.visit_compound(body)?;
        }

        Ok(())
    }

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult {
        for function_decl in &program.0 {
            self.visit_function_decl(function_decl)?;
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, var_decl: &VarDecl) -> Self::VarDeclResult {
        let ty = self.visit_expr(var_decl.init.as_ref().unwrap())?;
        if ty != var_decl.ty {
            Err(TypeCheckerError {
                token: var_decl.name.1.clone(),
                kind: TypeCheckerErrorKind::TypeMismatch {
                    expected: var_decl.ty.clone(),
                    got: ty,
                },
            })
        } else {
            self.add_symbol(var_decl.name.clone(), ty, true);
            Ok(())
        }
    }
}
