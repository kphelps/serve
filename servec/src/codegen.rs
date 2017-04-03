use aster::AstBuilder;
use itertools::Itertools;
use std::borrow::Borrow;
use std::collections::HashMap;
use serve_runtime::{exposed, ExposedFunctionRegistry, TypeSignature};
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

const ENV_TYPE: &'static str = "serve_runtime::environment::Environment";
const CREATE_ENV: &'static str = "serve_runtime::environment::Environment::new";
const SERVE_REF: &'static str = "serve_runtime::environment::EnvironmentReference";
const SERVE_STRING_TYPE: &'static str = "serve_runtime::types::ServeString";

#[derive(Clone)]
struct Scope {
    name: String,
    tipe: String,
}

impl Scope {
    fn new(tipe: &str, name: &str) -> Self {
        Self {
            name: name.to_string(),
            tipe: tipe.to_string(),
        }
    }
}

struct CodegenContext {
    named_scope: Vec<Scope>,
    definitions: Vec<PItem>,
    toplevel: Vec<syntax::ast::Stmt>,
    last: Vec<syntax::ast::Stmt>,

    exposed_registry: ExposedFunctionRegistry,
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

            exposed_registry: exposed(),
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
                self.with_scope("application", name, |ctx| {
                    ctx.codegen_vec(body, CodegenContext::codegen_statement)
                }).and(Ok(()))
            },
            Statement::Endpoint(ref name, ref params, ref return_type, ref body) => {
                let scope = self.get_scope()?.clone();
                let resolved_type = self.resolve_type(return_type)?.to_string();
                let action = self.static_environment.lookup_action(name)?;
                let response_serializer = self.static_environment.lookup_serializer(return_type)?;
                let endpoint_name = format!("endpoint_{}_{}", scope.name, name);
                self.toplevel.push(register_endpoint(
                    &scope.name,
                    &endpoint_name,
                    &action.method,
                    &action.path
                ));
                self.definitions.push(create_endpoint_epilogue(
                    &endpoint_name,
                    &resolved_type,
                    &response_serializer,
                ));
                self.definitions.push(create_endpoint(
                    &endpoint_name,
                    &resolved_type,
                ));
                self.definitions.push(create_endpoint_prologue(&endpoint_name, params));
                Ok(())
            },
            Statement::ItemFunctionCall(ref name, ref args) => {
                let res = self.handle_static_item_function(name, args);
                if res.is_ok() {
                    return res;
                }
                let exprs = self.codegen_vec(&args, CodegenContext::codegen_expression)?;
                let scope = self.get_scope()?.clone();
                let func_signature = self.get_exposed_function(&scope.tipe, name);
                if func_signature.is_some() {
                    self.toplevel.push(expr_to_stmt(create_method_call(&scope.name, name, exprs)));
                    Ok(())
                } else {
                    Err(
                        format!(
                            "Function '{}' not found in '{}' scope",
                            name,
                            scope.tipe
                        ).to_string()
                    )
                }
            },
            Statement::Serializer(ref tipe, ref body) => {
                let runtime_name = serializer_name(&self.full_scope_name(), tipe);
                let resolved_type = self.resolve_type(tipe)?.to_string();
                self.static_environment.register_serializer(tipe, &runtime_name);
                let body_exprs = self.with_scope("serializer", &runtime_name, |ctx| {
                    ctx.codegen_vec(&body, CodegenContext::codegen_expression)
                })?;
                self.definitions.push(create_serializer(&runtime_name, &resolved_type, &body_exprs));
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

    fn with_scope<F, R>(&mut self, scope_type: &str, name: &str, f: F) -> CodegenResult<R>
        where F: Fn(&mut CodegenContext) -> CodegenResult<R>
    {
        self.static_environment.push();
        self.named_scope.push(Scope::new(scope_type, name));
        let result = f(self);
        self.named_scope.pop();
        self.static_environment.pop();
        result
    }

    fn get_scope(&self) -> CodegenResult<&Scope> {
        Ok(self.named_scope.last().ok_or("No scope found".to_string())?)
    }

    fn full_scope_name(&self) -> String {
        self.named_scope.iter().map(|s| s.name.clone()).join("_")
    }

    fn get_exposed_function(&self, scope_type: &str, name: &str) -> Option<TypeSignature> {
        self.exposed_registry.lookup_scope(scope_type, name).map(Clone::clone)
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

    fn resolve_type(&self, name: &str) -> CodegenResult<&str> {
        let rust_type = match name {
            "String" => SERVE_STRING_TYPE,
            _ => return Err(format!("Could not resolve type '{:?}'", name))
        };
        Ok(rust_type)
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

fn create_serve_string_literal(s: &str) -> PExpr {
    AstBuilder::new().expr()
        .method_call("allocate")
        .id("env")
        .arg().build(create_string_literal(s))
        .build()
}

fn create_endpoint(
    name: &str,
    return_type: &str,
) -> PItem {
    let builder = AstBuilder::new();
    builder.item().fn_(name)
        // TODO: Should take its _actual_ arguments
        .with_args(vec![
            builder.arg().ref_mut_id("env").ty().id(ENV_TYPE),
            builder.arg().id("request").ty().id("hyper::server::Request"),
            builder.arg().id("response").ty().ref_().ty().id("hyper::server::Response"),
            builder.arg().id("captures").ty().id("reroute::Captures"),
        ])
        .return_().id(ref_type(return_type))
        .block()
        .build_expr(create_serve_string_literal("Hello World!"))
}

fn create_endpoint_prologue(
    endpoint_name: &str,
    args: &Vec<FunctionParameter>
) -> PItem {
    let builder = AstBuilder::new();
    builder.item().fn_(&endpoint_prologue_name(endpoint_name))
        .with_args(vec![
            builder.arg().id("request").ty().id("hyper::server::Request"),
            builder.arg().id("response").ty().id("hyper::server::Response"),
            builder.arg().id("captures").ty().id("reroute::Captures"),
        ])
        .default_return()
        .block()
        // TODO: Transform request data to function's arguments
        .stmt().let_id("user_response").expr().call().id(endpoint_name)
            .arg().call().id(CREATE_ENV).build()
            .arg().id("request")
            .arg().ref_().id("response")
            .arg().id("captures")
            .build()
        .build_expr(
            builder.expr().call().id(&endpoint_epilogue_name(endpoint_name))
                .arg().id("response")
                .arg().id("user_response")
                .build()
        )
}

fn create_endpoint_epilogue(
    endpoint_name: &str,
    return_type: &str,
    serializer: &str,
) -> PItem {
    let builder = AstBuilder::new();
    builder.item().fn_(&endpoint_epilogue_name(endpoint_name))
        .arg_id("response").ty().id("hyper::server::Response")
        .arg_id("user_response").ty().id(ref_type(return_type))
        .default_return()
        .block()
        .stmt().let_id("bytes").expr().call().id(serializer)
            .arg().id("user_response")
            .build()
        .build_expr(
            builder.expr().method_call("unwrap").build(
                create_method_call(
                    "response",
                    "send",
                    vec![builder.expr().ref_().id("bytes")],
                )
            ).build()
        )
}

fn register_endpoint(scope: &str, endpoint_name: &str, method: &str, path: &str) -> syntax::ast::Stmt {
    let e = create_method_call(scope, "route", vec![
        create_string_literal(method),
        create_string_literal(path),
        AstBuilder::new().expr().id(&endpoint_prologue_name(endpoint_name))
    ]);
    expr_to_stmt(e)
}

fn endpoint_prologue_name(name: &str) -> String {
    format!("prologue_{}", name)
}

fn endpoint_epilogue_name(name: &str) -> String {
    format!("epilogue_{}", name)
}

fn create_serializer(runtime_name: &str, tipe: &str, body: &Vec<PExpr>) -> PItem {
    let builder = AstBuilder::new();
    builder.item().fn_(runtime_name)
        .arg_id("input").ty().id(ref_type(tipe))
        .return_().id("Vec<u8>")
        .block()
        .build_expr(
            builder.expr()
                .method_call("into_bytes")
                    .method_call("to_string")
                        .lit().str("Hello World!")
                    .build()
                .build()
        )
}

fn serializer_name(scope: &str, name: &str) -> String {
    format!("serializer_{}_{}", name, scope)
}

fn expr_to_stmt(expr: PExpr) -> syntax::ast::Stmt {
    AstBuilder::new().stmt().build_expr(expr)
}

fn ref_type(tipe: &str) -> String {
    format!("{}<{}>", SERVE_REF, tipe)
}
