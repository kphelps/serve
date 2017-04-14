use serve_runtime;
use std::collections::HashMap;
use super::environment::Environment;
use super::ir::IRFragment;
use super::super::ast::*;
use super::super::symbol::{Symbol, SymbolRegistry};
use super::types::{
    MethodHeader,
    SemanticResult,
    ServeType,
    TypeContext,
    ValueEntry
};
use super::type_registrar::TypeRegistrar;

#[derive(Clone, Debug)]
pub struct BuiltinMetadata {
    rust_name: String,
    methods: HashMap<Symbol, MethodHeader>,
}

impl BuiltinMetadata {
    pub fn new(rust_name: &str) -> Self {
        Self {
            rust_name: rust_name.to_string(),
            methods: HashMap::new(),
        }
    }

    pub fn add_method(&mut self, name: Symbol, header: MethodHeader) {
        let result = self.methods.insert(name, header);
        assert!(result.is_none());
    }
}

#[derive(Debug)]
pub struct SemanticContext {
    pub environment: Environment,
    pub symbols: SymbolRegistry,
    pub type_context: Vec<TypeContext>,
    pub type_environments: HashMap<TypeContext, Environment>,
    pub builtins: HashMap<Symbol, BuiltinMetadata>,
    pub fragments: HashMap<Symbol, IRFragment>,
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
            type_environments: HashMap::new(),
            builtins: HashMap::new(),
            fragments: HashMap::new(),
        };
        ctx.register_type("Unit", ServeType::Unit);
        ctx.load_builtins().unwrap();

        let type_string = ctx.resolve_builtin_type("String").unwrap();
        let type_int = ctx.resolve_builtin_type("Int").unwrap();

        ctx.register_value("GET", ValueEntry::Variable(type_string.clone()));
        ctx.register_value("POST", ValueEntry::Variable(type_string.clone()));

        ctx.register_function_in_context(
            TypeContext::Application,
            "define_action",
            vec![
            // TODO: Should check the type signature dynamically based on the route.
                ServeType::Function(Vec::new(), Box::new(type_string.clone())),
                type_string.clone(),
                type_string.clone(),
            ],
            ServeType::Unit,
        );

        ctx
    }

    pub fn load_builtins(&mut self) -> Result<(), String> {
        let builtin_registry = serve_runtime::exposed();

        builtin_registry.each_type(|tipe| {
            let builtin_symbol = self.get_symbol(tipe.get_serve_name());
            self.register_type(
                tipe.get_serve_name(),
                ServeType::Builtin(builtin_symbol),
            );
            let metadata = BuiltinMetadata::new(tipe.get_rust_name());
            self.builtins.insert(builtin_symbol, metadata);
        });

        builtin_registry.each_type(|tipe| {
            let builtin_symbol = self.get_symbol(tipe.get_serve_name());
            let mut metadata = self.builtins.get(&builtin_symbol).unwrap().clone();
            tipe.each_method(|name, signature| {
                let args = signature.args.iter().map(|ty_name| {
                    self.resolve_builtin_type(ty_name).unwrap()
                }).collect();
                let ret = signature.return_type.as_ref()
                    .map(|ty_name| self.resolve_builtin_type(ty_name).unwrap())
                    .unwrap_or(ServeType::Unit);
                let symbol = self.get_symbol(name);
                metadata.add_method(symbol, MethodHeader::new(args, ret))
            });
            self.builtins.insert(builtin_symbol, metadata);
        });

        builtin_registry.each_scope(|scope, name, signature| {
            let ty_ctx = runtime_scope_to_type_context(scope);
            let args = signature.args.iter().map(|ty_name| {
                self.resolve_builtin_type(ty_name).unwrap()
            }).collect();
            let ret = signature.return_type.as_ref()
                .map(|ty_name| self.resolve_builtin_type(ty_name).unwrap())
                .unwrap_or(ServeType::Unit);

            self.register_function_in_context(
                ty_ctx,
                name,
                args,
                ret,
            ).unwrap();
        });

        Ok(())
    }

    pub fn resolve_method(&mut self, receiver: &ServeType, name: &Symbol)
        -> Result<&MethodHeader, String>
    {
        match *receiver {
            ServeType::Builtin(ref symbol) => {
                let receiver_name = self.get_name_from_symbol(*symbol);
                self.builtins.get(symbol)
                    .and_then(|builtin| builtin.methods.get(name))
                    .ok_or(format!("Method '{}' not found for receiver '{:?}'", name, receiver_name))
            }
            ref receiver_type => {
                let method_name = self.get_name_from_symbol(*name);
                Err(format!(
                    "Invalid receiver to {:?}: {:?}",
                    method_name,
                    receiver_type
                ))
            }
        }
    }

    pub fn resolve_builtin_type(&mut self, name: &str) -> SemanticResult {
        // Probably should cache this... but premature optimizations /shrug
        let symbol = self.get_symbol(name);
        if self.builtins.contains_key(&symbol) {
            Ok(ServeType::Builtin(symbol))
        } else {
            Err(format!("Builtin '{}' not found", name))
        }
    }

    pub fn get_symbol(&mut self, name: &str) -> Symbol {
        self.symbols.get_symbol(name)
    }

    pub fn get_name_from_symbol(&mut self, symbol: Symbol) -> Option<String> {
        self.symbols.get_name(&symbol)
    }

    pub fn with_context_scope<F, R>(&mut self, type_ctx: TypeContext, f: F) -> R
        where F: FnMut(&mut SemanticContext) -> R
    {
        self.type_context.push(type_ctx);
        let result = self.with_scope(f);
        self.type_context.pop();
        result
    }

    pub fn with_scope<F, R>(&mut self, mut f: F) -> R
        where F: FnMut(&mut SemanticContext) -> R
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

fn runtime_scope_to_type_context(scope: &str) -> TypeContext {
    match scope {
        "application" => TypeContext::Application,
        _ => panic!(format!("Unknown runtime scope: {}", scope))
    }
}
