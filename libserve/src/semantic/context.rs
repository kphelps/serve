use std::collections::HashMap;
use super::environment::Environment;
use super::super::ast::*;
use super::super::symbol::{Symbol, SymbolRegistry};
use super::types::{SemanticResult, ServeType, TypeContext, ValueEntry};
use super::type_registrar::TypeRegistrar;

#[derive(Debug)]
pub struct SemanticContext {
    pub environment: Environment,
    pub symbols: SymbolRegistry,
    pub type_context: Vec<TypeContext>,
    pub type_environments: HashMap<TypeContext, Environment>,
}

pub trait Symbolizable {
    fn to_symbol(&self, ctx: &mut SemanticContext) -> Symbol;
}

impl Symbolizable for Symbol {
    fn to_symbol(&self, _: &mut SemanticContext) -> Symbol {
        self.clone()
    }
}

impl Symbolizable for String
{
    fn to_symbol(&self, ctx: &mut SemanticContext) -> Symbol {
        ctx.get_symbol(&self)
    }
}

impl<'a> Symbolizable for &'a str
{
    fn to_symbol(&self, ctx: &mut SemanticContext) -> Symbol {
        ctx.get_symbol(self)
    }
}

impl SemanticContext {

    pub fn new(symbols: SymbolRegistry) -> Self {
        let mut ctx = Self {
            environment: Environment::new(),
            symbols: symbols,
            type_context: Vec::new(),
            type_environments: HashMap::new()
        };
        ctx.register_type("String", ServeType::String);
        ctx.register_type("Int", ServeType::Int);
        ctx.register_type("Unit", ServeType::Unit);

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

    pub fn get_symbol(&mut self, name: &str) -> Symbol {
        self.symbols.get_symbol(name)
    }

    pub fn get_name_from_symbol(&mut self, symbol: Symbol) -> Option<String> {
        self.symbols.get_name(&symbol)
    }

    pub fn with_context_scope<F>(&mut self, type_ctx: TypeContext, f: F) -> SemanticResult
        where F: Fn(&mut SemanticContext) -> SemanticResult
    {
        self.type_context.push(type_ctx);
        let result = self.with_scope(f);
        self.type_context.pop();
        result
    }

    pub fn with_scope<F>(&mut self, f: F) -> SemanticResult
        where F: Fn(&mut SemanticContext) -> SemanticResult
    {
        self.environment.enter_scope();
        let result = f(self);
        self.environment.leave_scope();
        result
    }

    pub fn extract_parameter_types(&mut self, args: &Vec<FunctionParameter>) -> Vec<ServeType> {
        args.iter().map(FunctionParameter::get_type)
            .map(|fpt| self.get_type(fpt))
            .collect()
    }
}
