use aster::AstBuilder;
use itertools::Itertools;
use std::borrow::Borrow;
use std::collections::HashMap;
use super::ast::*;
use super::static_environment::StaticEnvironment;
use syntax;
use syntax::ptr::P;

type CodegenResult<T> = Result<T, String>;
type CodegenAction = CodegenResult<()>;

type PExpr = P<syntax::ast::Expr>;
type PItem = P<syntax::ast::Item>;

const RT: &'static str = "serve_runtime";

type StaticItemFunction = Fn(&mut CodegenContext, &Vec<Expression>) -> CodegenAction + Send + Sync;
type BoxedStaticItemFunction = Box<StaticItemFunction>;

struct CodegenContext {
    named_scope: Vec<String>,
    definitions: Vec<PItem>,
    toplevel: Vec<syntax::ast::Stmt>,
    last: Vec<syntax::ast::Stmt>,

    static_environment: StaticEnvironment,
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
            definitions: Vec::new(),
            toplevel: Vec::new(),
            last: Vec::new(),

            static_environment: StaticEnvironment::new(),
        }
    }

    fn emit(mut self) -> String {
        let builder = AstBuilder::new();
        let module_attrs = vec![
            builder.attr().inner().allow(vec!["non_snake_case"]),
        ];
        let mut items = Vec::new();
        items.push(builder.item().extern_crate("hyper").build());
        items.push(builder.item().extern_crate("reroute").build());
        items.push(builder.item().extern_crate("serve_runtime").build());
        items.append(&mut self.definitions);
        let main = builder.item().fn_("main").default_return().block()
            .with_stmts(self.toplevel)
            .with_stmts(self.last)
            .build();
        items.push(main);
        vec![
            module_attrs.iter().map(|a| syntax::print::pprust::attr_to_string(a)).join("\n"),
            items.iter().map(|i| syntax::print::pprust::item_to_string(i)).join("\n"),
        ].iter().join("\n")
    }

    fn codegen(&mut self, ast: &AST) -> CodegenAction {
        self.codegen_vec(ast, CodegenContext::codegen_statement).and(Ok(()))
    }

    fn codegen_statement(&mut self, statement: &Statement) -> CodegenAction {
        match *statement {
            Statement::Application(ref name, ref body) => {
                self.last.push(expr_to_stmt(create_method_call(name, "start", vec![])));
                self.toplevel.push(create_app(name));
                self.with_scope(name, |ctx| {
                    ctx.codegen_vec(body, CodegenContext::codegen_statement)
                }).and(Ok(()))
            },
            Statement::Endpoint(ref name, ref params, ref return_type, ref body) => {
                let scope = self.get_scope()?;
                let action = self.static_environment.lookup_action(name)?;
                let endpoint_name = format!("endpoint_{}_{}", scope, name);
                self.toplevel.push(register_endpoint(
                    &scope,
                    &endpoint_name,
                    &action.method,
                    &action.path
                ));
                self.definitions.push(create_endpoint(&endpoint_name/*, params, return_type, body*/));
                Ok(())
            },
            Statement::ItemFunctionCall(ref name, ref args) => {
                let res = self.handle_static_item_function(name, args);
                if res.is_ok() {
                    return res;
                }
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

    fn handle_static_item_function(&mut self, name: &str, args: &Vec<Expression>)
        -> CodegenAction
    {
        match name {
            "define_action" => self.define_action(args),
            _ => Err(format!("Invalid static item function")),
        }
    }

    fn define_action(&mut self, args: &Vec<Expression>) -> CodegenAction {
        match &args[..] {
            &[
                Expression::Identifier(ref name),
                Expression::Identifier(ref method),
                Expression::StringLiteral(ref path)
            ] => {
                self.static_environment.register_action(name, method, path);
                Ok(())
            },
            _ => Err(format!("Invalid args for define_action")),
        }
    }
}

fn create_app(name: &str) -> syntax::ast::Stmt {
    AstBuilder::new().stmt()
        .let_().mut_id(name).expr().call()
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

fn create_endpoint(
    name: &str,
) -> PItem {
    let builder = AstBuilder::new();
    builder.item().fn_(name)
        .with_args(vec![
            builder.arg().id("request").ty().id("hyper::server::Request"),
            builder.arg().id("response").ty().id("hyper::server::Response"),
            builder.arg().id("captures").ty().id("reroute::Captures"),
        ])
        .default_return()
        .block()
        .build_expr(
            builder.expr().method_call("unwrap").build(
                create_method_call(
                    "response",
                    "send",
                    vec![builder.expr().method_call("as_bytes").lit().str("Hello World!").build()],
                )
            ).build()
        )
}

fn register_endpoint(scope: &str, name: &str, method: &str, path: &str) -> syntax::ast::Stmt {
    let e = create_method_call(scope, "route", vec![
        create_string_literal(method),
        create_string_literal(path),
        AstBuilder::new().expr().id(name)
    ]);
    expr_to_stmt(e)
}

fn expr_to_stmt(expr: PExpr) -> syntax::ast::Stmt {
    AstBuilder::new().stmt().build_expr(expr)
}
