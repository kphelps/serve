use std::collections::HashMap;
use super::ast::*;
use super::symbol::{Symbol, SymbolBindingTable, SymbolRegistry};


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServeType {
    Unit,
    Int,
    String,
    Record(String, Vec<ServeType>),
    Array(Box<ServeType>),
    Function(Vec<ServeType>, Box<ServeType>),
    Placeholder(Symbol),
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ValueEntry {
    Variable(ServeType),
    Function(Vec<ServeType>, ServeType),
    Endpoint(Vec<ServeType>, ServeType),
}

impl ValueEntry {

    fn get_type(&self) -> SemanticAnalysisResult {
        match *self {
            ValueEntry::Variable(ref tipe) => Ok(tipe.clone()),
            ValueEntry::Function(ref args, ref resp) => {
                Ok(ServeType::Function(args.clone(), Box::new(resp.clone())))
            },
            ValueEntry::Endpoint(ref args, ref resp) => {
                Ok(ServeType::Function(args.clone(), Box::new(resp.clone())))
            },
            ref t => Err(format!("Invalid type '{:?}'", t)),
        }
    }
}

#[derive(Debug)]
struct Environment {
    types: SymbolBindingTable<ServeType>,
    values: SymbolBindingTable<ValueEntry>,
}

impl Environment {
    fn new() -> Self {
        Self {
            types: SymbolBindingTable::new(),
            values: SymbolBindingTable::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.types.enter_scope();
        self.values.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.types.leave_scope();
        self.values.leave_scope();
    }

    fn get_value(&self, symbol: Symbol) -> Option<&ValueEntry> {
        self.values.get(symbol)
    }

    fn insert_value(&mut self, symbol: Symbol, entry: ValueEntry) {
        self.values.insert(symbol, entry)
    }

    fn get_type(&self, symbol: Symbol) -> Option<&ServeType> {
        self.types.get(symbol)
    }

    fn insert_type(&mut self, symbol: Symbol, entry: ServeType) {
        self.types.insert(symbol, entry)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum TypeContext {
    Global,
    Application,
    Endpoint,
    Serializer
}

pub type SemanticAnalysisResult = Result<ServeType, String>;

#[derive(Debug)]
struct SemanticAnalysisContext {
    environment: Environment,
    symbols: SymbolRegistry,
    type_context: Vec<TypeContext>,
    type_environments: HashMap<TypeContext, Environment>,
}

trait Symbolizable {
    fn to_symbol(&self, ctx: &mut SemanticAnalysisContext) -> Symbol;
}

impl Symbolizable for Symbol {
    fn to_symbol(&self, _: &mut SemanticAnalysisContext) -> Symbol {
        self.clone()
    }
}

impl Symbolizable for String
{
    fn to_symbol(&self, ctx: &mut SemanticAnalysisContext) -> Symbol {
        ctx.get_symbol(&self)
    }
}

impl<'a> Symbolizable for &'a str
{
    fn to_symbol(&self, ctx: &mut SemanticAnalysisContext) -> Symbol {
        ctx.get_symbol(self)
    }
}

impl SemanticAnalysisContext {

    pub fn new(symbols: SymbolRegistry) -> Self {
        let mut ctx = Self {
            environment: Environment::new(),
            symbols: symbols,
            type_context: Vec::new(),
            type_environments: HashMap::new()
        };
        ctx.register_type("String", ServeType::String);
        ctx.register_type("Int", ServeType::Int);

        ctx.register_value("GET", ValueEntry::Variable(ServeType::String));
        ctx.register_value("POST", ValueEntry::Variable(ServeType::String));

        ctx.register_function_in_context(
            TypeContext::Application,
            "host",
            vec![ServeType::String],
            ServeType::Unit,
        );
        ctx.register_function_in_context(
            TypeContext::Application,
            "port",
            vec![ServeType::Int],
            ServeType::Unit,
        );
        ctx.register_function_in_context(
            TypeContext::Application,
            "define_action",
            vec![
            // TODO: Should check the type signature dynamically based on the route.
                ServeType::Function(Vec::new(), Box::new(ServeType::String)),
                ServeType::String,
                ServeType::String
            ],
            ServeType::Unit,
        );

        ctx
    }

    fn get_symbol(&mut self, name: &str) -> Symbol {
        self.symbols.get_symbol(name)
    }

    fn register_type<S: Symbolizable>(&mut self, symbolizable: S, value: ServeType) {
        let symbol = symbolizable.to_symbol(self);
        self.environment.insert_type(symbol, value)
    }

    fn register_value<S: Symbolizable>(&mut self, symbolizable: S, value: ValueEntry) {
        let symbol = symbolizable.to_symbol(self);
        self.environment.insert_value(symbol, value)
    }

    fn register_function_in_context<S: Symbolizable>(
        &mut self,
        ctx: TypeContext,
        symbol: S,
        args: Vec<ServeType>,
        return_type: ServeType
    ) {
        self.register_value_in_context(
            ctx,
            symbol,
            ValueEntry::Variable(
                ServeType::Function(args, Box::new(return_type))
            )
        )
    }

    fn register_value_in_context<S: Symbolizable>(
        &mut self,
        ctx: TypeContext,
        symbolizable: S,
        value: ValueEntry
    ) {
        let symbol = symbolizable.to_symbol(self);
        self.type_environments.entry(ctx)
            .or_insert_with(Environment::new)
            .insert_value(symbol, value)
    }

    fn get_current_context(&self) -> TypeContext {
        self.type_context.last().unwrap_or(&TypeContext::Global).clone()
    }

    fn get_value_from_context(&self, ctx: TypeContext, symbol: Symbol) -> Option<&ValueEntry> {
        self.type_environments.get(&ctx)
            .and_then(|env| env.get_value(symbol))
    }

    fn get_value_from_current_context(&self, symbol: Symbol) -> Option<&ValueEntry> {
        let ctx = self.get_current_context();
        self.get_value_from_context(ctx, symbol)
    }

    fn get_value(&self, symbol: Symbol) -> Option<ValueEntry> {
        self.environment.get_value(symbol)
            .or_else(|| self.get_value_from_current_context(symbol))
            .map(Clone::clone)
    }

    fn get_type(&self, symbol: Symbol) -> ServeType {
        self.environment.get_type(symbol).unwrap().clone()
    }

    pub fn scan_declarations(&mut self, decls: &Vec<Declaration>) {
        for decl in decls {
            self.scan_declaration(decl);
        }
    }

    pub fn scan_declaration(&mut self, decl: &Declaration) {
        match *decl {
            Declaration::Application(ref name, ref body) => {
                self.scan_application_statements(body)
            },
            _ => (),
        }
    }

    pub fn scan_application_statements(&mut self, stmts: &Vec<ApplicationStatement>) {
        for stmt in stmts {
            self.scan_application_statement(stmt);
        }
    }

    pub fn scan_application_statement(&mut self, stmt: &ApplicationStatement) {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref return_type_name, ref body) => {
                let arg_types = self.extract_parameter_types(args);
                let return_type = self.get_type(*return_type_name);
                self.environment.insert_value(
                    *name,
                    ValueEntry::Endpoint(arg_types, return_type)
                );
            },
            _ => (),
        }
    }

    pub fn type_check_declarations(&mut self, decls: &Vec<Declaration>) -> SemanticAnalysisResult {
        let mut result = ServeType::Unit;
        for decl in decls {
            result = self.type_check_declaration(decl)?;
        }
        Ok(result)
    }

    pub fn type_check_declaration(&mut self, decl: &Declaration) -> SemanticAnalysisResult {
        match *decl {
            Declaration::Application(ref name, ref body) => {
                self.with_scope(TypeContext::Application, |ctx| {
                    ctx.type_check_application_statements(body)
                })
            },
            Declaration::Serializer(ref name, ref body) => {
                self.with_scope(TypeContext::Serializer, |ctx| {
                    ctx.type_check_expressions(body)
                })
            },
        }
    }

    pub fn type_check_application_statements(&mut self, stmts: &Vec<ApplicationStatement>)
        -> SemanticAnalysisResult
    {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_application_statement(stmt)?;
        }
        Ok(result)
    }

    pub fn type_check_application_statement(&mut self, stmt: &ApplicationStatement)
        -> SemanticAnalysisResult
    {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref return_type_name, ref body) => {
                // TODO: expose endpoint in the result?
                let actual_return = self.with_scope(TypeContext::Endpoint, |ctx| {
                    for arg in args {
                        let arg_type = ctx.get_type(arg.get_type());
                        ctx.register_value(
                            arg.get_name(),
                            ValueEntry::Variable(arg_type)
                        );
                    }
                    ctx.type_check_expressions(body)
                })?;
                let return_type = self.get_type(*return_type_name);
                self.check_types(&return_type, actual_return)
            },
            ApplicationStatement::ItemFunctionCall(ref name, ref args) => {
                self.type_check_function_call(name, args)
            }
        }
    }

    pub fn type_check_expressions(&mut self, exprs: &Vec<Expression>)
        -> SemanticAnalysisResult
    {
        let mut result = ServeType::Unit;
        for expr in exprs {
            result = self.type_check_expression(expr)?;
        }
        Ok(result)
    }

    pub fn type_check_expression(&mut self, expr: &Expression)
        -> SemanticAnalysisResult
    {
        match *expr {
            Expression::FunctionCall(ref name, ref args) => {
                self.type_check_function_call(name, args)
            },
            Expression::Return(ref return_expr) => {
                self.type_check_expression(return_expr)
            },
            Expression::IntLiteral(_) => {
                Ok(ServeType::Int)
            },
            Expression::StringLiteral(_) => {
                Ok(ServeType::String)
            },
            Expression::Identifier(ref name) => {
                self.get_value(*name)
                    .ok_or(format!("Undeclared identifier '{}'", self.symbols.get_name(name).unwrap()))
                    .and_then(|v| ValueEntry::get_type(&v))
            },
            _ => Err("Not implemented yet".to_string()),
        }
    }

    fn type_check_function_call(&mut self, name: &Symbol, args: &Vec<Expression>)
        -> SemanticAnalysisResult
    {
        let call_arg_types = args.iter()
            .fold(Ok(Vec::new()), |acc, arg| {
                if acc.is_err() {
                    return acc
                }
                self.type_check_expression(arg)
                    .map(|expr_arg| {
                        let mut v = acc.unwrap();
                        v.push(expr_arg);
                        v
                    })
            })?;
        self.get_value(*name)
            .ok_or(format!("Undeclared function '{}'", self.symbols.get_name(name).unwrap()))
            .and_then(|val| {
                match val {
                    ValueEntry::Variable(ServeType::Function(ref fn_arg_types, ref fn_return)) => {
                        if fn_arg_types == &call_arg_types {
                            Ok(*fn_return.clone())
                        } else {
                            Err(format!(
                                "Invalid function call '{:?}'. Expected '{:?}'",
                                call_arg_types,
                                fn_arg_types
                            ))
                        }
                    },
                    ref other => Err(format!("Expected callable, found '{:?}'", other)),
                }
            })
    }

    fn check_types(&mut self, expected: &ServeType, actual: ServeType)
        -> SemanticAnalysisResult
    {
        if expected == &actual {
            Ok(actual)
        } else {
            Err(format!("Expected '{:?}', found '{:?}'", expected, actual))
        }
    }

    fn with_scope<F>(&mut self, type_ctx: TypeContext, f: F) -> SemanticAnalysisResult
        where F: Fn(&mut SemanticAnalysisContext) -> SemanticAnalysisResult
    {
        self.type_context.push(type_ctx);
        self.environment.enter_scope();
        let result = f(self);
        self.environment.leave_scope();
        self.type_context.pop();
        result
    }

    fn extract_parameter_types(&mut self, args: &Vec<FunctionParameter>) -> Vec<ServeType> {
        args.iter().map(FunctionParameter::get_type)
            .map(|fpt| self.get_type(fpt))
            .collect()
    }

}

pub fn type_check(
    ast: AST,
    symbols: SymbolRegistry,
) -> SemanticAnalysisResult {
    let mut ctx = SemanticAnalysisContext::new(symbols);
    ctx.scan_declarations(&ast);
    ctx.type_check_declarations(&ast)
}
