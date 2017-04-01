use aster::AstBuilder;
use itertools::Itertools;
use std::borrow::Borrow;
use std::collections::HashMap;
use super::ast::*;
use syntax;
use syntax::ptr::P;

type CodegenResult<T> = Result<T, String>;
type CodegenAction = CodegenResult<()>;

type PExpr = P<syntax::ast::Expr>;

const RT: &'static str = "serve_runtime";

struct CodegenContext {
    named_scope: Vec<String>,
    toplevel: Vec<syntax::ast::Stmt>,
    definitions: Vec<syntax::ast::Stmt>,
}

pub fn codegen(ast: AST) -> CodegenResult<String> {
    let mut context = CodegenContext::new();
    context.codegen(&ast)?;
    Ok(context.emit())
}

impl CodegenContext {

    fn new() -> Self {
        Self {
            named_scope: Vec::new(),
            toplevel: Vec::new(),
            definitions: Vec::new(),
        }
    }

    fn emit(self) -> String {
        let main = AstBuilder::new().item().fn_("main")
            .default_return()
            .block().with_stmts(self.toplevel)
            .build();
        syntax::print::pprust::item_to_string(&main)
    }

    fn codegen(&mut self, ast: &AST) -> CodegenAction {
        self.codegen_vec(ast, CodegenContext::codegen_statement).and(Ok(()))
    }

    fn codegen_statement(&mut self, statement: &Statement) -> CodegenAction {
        match *statement {
            Statement::Application(ref name, ref body) => {
                self.toplevel.push(create_app(name));
                self.with_scope(name, |ctx| {
                    ctx.codegen_vec(body, CodegenContext::codegen_statement)
                }).and(Ok(()))
            },
            Statement::Endpoint(ref name, ref params, ref return_type, ref body) => {
                Ok(())
            },
            Statement::ItemFunctionCall(ref name, ref args) => {
                let exprs = self.codegen_vec(&args, CodegenContext::codegen_expression)?;
                let scope = self.get_scope()?;
                self.toplevel.push(expr_to_stmt(create_method_call(&scope, name, exprs)));
                Ok(())
            },
        }
    }

    fn codegen_expression(&mut self, expr: &Expression)
        -> CodegenResult<PExpr>
    {
        match *expr {
            Expression::FunctionCall(ref name, ref args) => {
                let exprs = self.codegen_vec(&args, CodegenContext::codegen_expression)?;
                Ok(create_call(name, exprs))
            },
            Expression::Return(ref expr) => {
                let to_return = self.codegen_expression(&expr)?;
                Ok(create_return(to_return))
            }
            Expression::IntLiteral(n) => Ok(create_int_literal(n)),
            Expression::StringLiteral(ref s) => Ok(create_string_literal(s)),
            Expression::Identifier(ref id) => Ok(create_identifier(id)),
        }
    }

    fn with_scope<F, R>(&mut self, name: &str, f: F) -> CodegenResult<R>
        where F: Fn(&mut CodegenContext) -> CodegenResult<R>
    {
        self.named_scope.push(name.to_owned());
        let result = f(self);
        self.named_scope.pop();
        result
    }

    fn get_scope(&self) -> CodegenResult<String> {
        Ok(self.named_scope.last().ok_or("No scope found".to_string())?.to_owned())
    }

    fn codegen_vec<T, F, R>(&mut self, v: &Vec<T>, f: F) -> CodegenResult<Vec<R>>
        where F: Fn(&mut CodegenContext, &T) -> CodegenResult<R>
    {
        v.iter().map(|node| f(self, node)).collect()
    }
}

fn create_app(name: &str) -> syntax::ast::Stmt {
    AstBuilder::new().stmt()
        .let_id(name).expr().call()
        .path().ids(vec![RT, "App", "new"]).build()
        .build()
}

fn create_method_call(receiver: &str, name: &str, args: Vec<PExpr>)
    -> PExpr
{
    let call = AstBuilder::new().expr().method_call(name).id(receiver);
    args.iter().fold(call, |acc, arg| {
        acc.arg().build(arg.clone())
    }).build()
}

fn create_call(name: &str, args: Vec<PExpr>) -> PExpr {
    let call = AstBuilder::new().expr().call().id(name);
    args.iter().fold(call, |acc, arg| {
        acc.arg().build(arg.clone())
    }).build()
}

fn create_return(to_return: PExpr) -> PExpr {
    AstBuilder::new().expr().return_expr().build(to_return)
}

fn create_int_literal(n: i64) -> PExpr {
    AstBuilder::new().expr().i64(n)
}

fn create_string_literal(s: &str) -> PExpr {
    AstBuilder::new().expr().lit().str(s)
}

fn create_identifier(id: &str) -> PExpr {
    AstBuilder::new().expr().id(id)
}

fn expr_to_stmt(expr: PExpr) -> syntax::ast::Stmt {
    AstBuilder::new().stmt().build_expr(expr)
}
