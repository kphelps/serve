use aster::AstBuilder;
use itertools::Itertools;
use std::borrow::Borrow;
use std::collections::HashMap;
use serve_runtime::{
    exposed,
    ExposedFunctionRegistry,
    ExposedType,
    TypeSignature
};
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

const ENV_TYPE: &'static str = "serve_runtime::environment::Env";
const CREATE_ENV: &'static str = "serve_runtime::environment::Env::new";
const SERVE_REF: &'static str = "serve_runtime::environment::EnvRef";

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

struct TopLevelInitializer {
    tipe: String,
    name: String,
    body: Vec<syntax::ast::Stmt>,
}

impl TopLevelInitializer {
    pub fn new(name: &str, tipe: &str) -> Self {
        Self {
            tipe: tipe.to_string(),
            name: name.to_string(),
            body: Vec::new(),
        }
    }

    pub fn extend_body(&mut self, stmts: Vec<syntax::ast::Stmt>)
    {
        for stmt in stmts {
            self.append_body(stmt);
        }
    }

    pub fn append_body(&mut self, stmt: syntax::ast::Stmt) {
        self.body.push(stmt);
    }

    pub fn codegen(&self)
        -> CodegenResult<Vec<syntax::ast::Stmt>>
    {
        let mut statements = Vec::new();
        statements.push(self.create_constructor()?);
        statements.push(
            AstBuilder::new().stmt().expr()
                .method_call("with_env")
                .id(&self.name)
                .arg().closure().fn_decl()
                    .arg_ref_mut_id("env").ty().infer()
                    .arg_ref_mut_id(&self.name).ty().infer()
                    .default_return()
                    .expr().block()
                    .with_stmts(self.body.clone())
                    .build()
                .build()
        );
        statements.push(expr_to_stmt(create_method_call(&self.name, "start", vec![])));
        Ok(statements)
    }

    fn create_constructor(&self) -> CodegenResult<syntax::ast::Stmt> {
        let result = match self.tipe.as_ref() {
            "application" => create_app(&self.name),
            _ => return Err(format!("Invalid initializer type '{}'", self.tipe)),
        };
        Ok(result)
    }
}

struct TemporaryGenerator {
    counter: usize,
}

impl TemporaryGenerator {
    fn new() -> Self {
        Self {
            counter: 0,
        }
    }

    fn temp(&mut self) -> String {
        let result = format!("_serve_temp_{}", self.counter);
        self.counter += 1;
        result
    }
}

struct CodegenContext {
    named_scope: Vec<Scope>,
    definitions: Vec<PItem>,
    initializers: HashMap<String, TopLevelInitializer>,
    last: Vec<syntax::ast::Stmt>,

    exposed_registry: ExposedFunctionRegistry,
    static_environment: StaticEnvironment,
    temporaries: TemporaryGenerator,
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
            initializers: HashMap::new(),
            last: Vec::new(),

            exposed_registry: exposed(),
            static_environment: StaticEnvironment::new(),
            temporaries: TemporaryGenerator::new(),
        }
    }

    fn emit(mut self) -> String {
        let builder = AstBuilder::new();
        let module_attrs = vec![
            builder.attr().inner().allow(vec!["non_snake_case"]),
            builder.attr().inner().allow(vec!["unused_variables"]),
        ];
        let mut items = Vec::new();
        items.push(builder.item().extern_crate("hyper").build());
        items.push(builder.item().extern_crate("reroute").build());
        items.push(builder.item().extern_crate("serve_runtime").build());
        items.append(&mut self.definitions);
        let main = self.initializers.iter().fold(
            builder.item().fn_("main").default_return().block(),
            |acc, (_, initializer)| { acc.with_stmts(initializer.codegen().unwrap()) });
        items.push(main.build());
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
                assert!(self.get_scope().is_err());
                let initializer = TopLevelInitializer::new(name, "application");
                self.initializers.insert(name.to_string(), initializer);
                self.with_scope("application", name, |ctx| {
                    ctx.codegen_vec(body, CodegenContext::codegen_statement)
                })?;
                Ok(())
            },
            Statement::Endpoint(ref name, ref params, ref return_type, ref body) => {
                let scope = self.get_scope()?.clone();
                let resolved_type = self.resolve_type(return_type)?.to_string();
                let action = self.static_environment.lookup_action(name)?;
                let response_serializer = self.static_environment.lookup_serializer(return_type)?;
                let endpoint_name = format!("endpoint_{}_{}", scope.name, name);
                let body_exprs = self.codegen_body(body)?;
                let initializer = self.initializers.get_mut(&scope.name)
                    .ok_or("Initializer not found")?;
                initializer.append_body(register_endpoint(
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
                    &body_exprs,
                ));
                self.definitions.push(create_endpoint_prologue(&endpoint_name, params));
                Ok(())
            },
            Statement::ItemFunctionCall(ref name, ref args) => {
                let res = self.handle_static_item_function(name, args);
                if res.is_ok() {
                    return res;
                }
                let (ids, stmts) = self.codegen_eval_args(&args)?;
                let scope = self.get_scope()?.clone();
                let func_signature = self.get_exposed_function(&scope.tipe, name)
                    .ok_or(format!("Function '{}' not found in '{}' scope", name, scope.tipe))?.clone();
                let initializer = self.initializers.get_mut(&scope.name)
                    .ok_or(format!("Initializer '{}' not found", scope.name))?;
                initializer.extend_body(stmts);
                initializer.append_body(expr_to_stmt(create_method_call_with_env(&scope.name, name, ids)));
                Ok(())
            },
            Statement::Serializer(ref tipe, ref body) => {
                let runtime_name = serializer_name(&self.full_scope_name(), tipe);
                let resolved_type = self.resolve_type(tipe)?.to_string();
                self.static_environment.register_serializer(tipe, &runtime_name);
                let body_exprs = self.with_scope("serializer", &runtime_name, |ctx| {
                    ctx.codegen_body(&body)
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
            Expression::IntLiteral(n) => Ok(create_serve_int_literal(n)),
            Expression::StringLiteral(ref s) => Ok(create_serve_string_literal(s)),
            Expression::Identifier(ref id) => Ok(create_identifier(id)),
            Expression::MethodCall(ref receiver, ref name, ref args) => {
                let receiver_expr = self.codegen_expression(receiver)?;
                let args = self.codegen_vec(&args, CodegenContext::codegen_expression)?;
                Ok(create_expr_method_call(receiver_expr, name, args))
            },
            ref a => Err(format!("Expression not implemented: {:?}", a)),
        }
    }

    fn codegen_eval_args(&mut self, args: &Vec<Expression>)
        -> CodegenResult<(Vec<PExpr>, Vec<syntax::ast::Stmt>)>
    {
        let mut ids = Vec::new();
        let mut stmts = Vec::new();
        for arg in args {
            let temp = self.temporaries.temp();
            ids.push(create_identifier(&temp));
            let expr = self.codegen_expression(arg)?;
            stmts.push(create_assignment(&temp, expr));
        }
        Ok((ids, stmts))
    }

    fn codegen_body(&mut self, exprs: &Vec<Expression>)
        -> CodegenResult<Vec<PExpr>>
    {
        self.codegen_vec(exprs, CodegenContext::codegen_expression)
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
        self.exposed_registry.lookup_type(name)
            .map(ExposedType::get_rust_name)
            .ok_or(format!("Could not resolve type '{}'", name))
    }
}

fn create_app(name: &str) -> syntax::ast::Stmt {
    AstBuilder::new().stmt()
        .let_().mut_id(name).expr().call()
        .path().ids(vec![RT, "App", "new"]).build()
        .build()
}

fn create_expr_method_call(receiver: PExpr, name: &str, args: Vec<PExpr>)
    -> PExpr
{
    AstBuilder::new().expr().method_call(name).build(receiver).with_args(args).build()
}

fn create_method_call(receiver: &str, name: &str, args: Vec<PExpr>)
    -> PExpr
{
    create_expr_method_call(create_identifier(receiver), name, args)
}

fn create_method_call_with_env(receiver: &str, name: &str, mut args: Vec<PExpr>)
    -> PExpr
{
    args.insert(0, create_identifier("env"));
    create_method_call(receiver, name, args)
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

fn create_assignment(id: &str, rhs: PExpr) -> syntax::ast::Stmt {
    AstBuilder::new().stmt().let_id(id).build_expr(rhs)
}

fn allocate(to_allocate: PExpr) -> PExpr {
    AstBuilder::new().expr()
        .method_call("allocate")
        .id("env")
        .arg().build(to_allocate)
        .build()
}

fn create_serve_int_literal(n: i64) -> PExpr {
    allocate(create_int_literal(n))
}

fn create_serve_string_literal(s: &str) -> PExpr {
    allocate(create_string_literal(s))
}

fn create_endpoint(
    name: &str,
    return_type: &str,
    body: &Vec<PExpr>,
) -> PItem {
    let builder = AstBuilder::new();
    let body_stmts = body.iter().cloned().map(expr_to_stmt);
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
        .with_stmts(body_stmts)
        .build()
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
