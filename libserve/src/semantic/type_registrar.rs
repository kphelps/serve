use std::fmt::Debug;
use super::context::{SemanticContext, Symbolizable};
use super::environment::Environment;
use super::super::symbol::Symbol;
use super::types::{ServeType, TypeContext, ValueEntry};

pub type InsertResult = Result<(), String>;

pub trait TypeRegistrar {
    fn register_type<S: Symbolizable>(
        &mut self,
        symbolizable: S,
        value: ServeType
    ) -> InsertResult;

    fn register_value<S: Symbolizable>(
        &mut self,
        symbolizable: S,
        value: ValueEntry
    ) -> InsertResult;

    fn register_function_in_context<S: Symbolizable>(
        &mut self,
        ctx: TypeContext,
        symbol: S,
        args: Vec<ServeType>,
        return_type: ServeType
    ) -> InsertResult;

    fn register_value_in_context<S: Symbolizable>(
        &mut self,
        ctx: TypeContext,
        symbolizable: S,
        value: ValueEntry
    ) -> InsertResult;

    fn get_current_context(&self) -> TypeContext;

    fn get_value_from_context(&self, ctx: TypeContext, symbol: Symbol) -> Option<&ValueEntry>;

    fn get_value_from_current_context(&self, symbol: Symbol) -> Option<&ValueEntry>;

    fn get_value(&self, symbol: Symbol) -> Option<ValueEntry>;

    fn get_type(&self, symbol: Symbol) -> ServeType;

}

impl TypeRegistrar for SemanticContext {

    fn register_type<S: Symbolizable>(
        &mut self,
        symbolizable: S,
        value: ServeType
    ) -> InsertResult
    {
        let symbol = symbolizable.to_symbol(self);
        let result = self.environment.insert_type(symbol, value);
        self.handle_insert_error("type", TypeContext::Global, symbol, result)
    }

    fn register_value<S: Symbolizable>(
        &mut self,
        symbolizable: S,
        value: ValueEntry
    ) -> InsertResult
    {
        let symbol = symbolizable.to_symbol(self);
        let result = self.environment.insert_value(symbol, value);
        self.handle_insert_error("value", TypeContext::Global, symbol, result)
    }

    fn register_function_in_context<S: Symbolizable>(
        &mut self,
        ctx: TypeContext,
        symbol: S,
        args: Vec<ServeType>,
        return_type: ServeType
    ) -> InsertResult
    {
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
    ) -> InsertResult
    {
        let symbol = symbolizable.to_symbol(self);
        let result = self.type_environments.entry(ctx.clone())
            .or_insert_with(Environment::new)
            .insert_value(symbol, value);
        self.handle_insert_error("value", ctx, symbol, result)
    }

    fn get_current_context(&self) -> TypeContext {
        self.type_context.last().unwrap_or(&TypeContext::Global).clone()
    }

    fn get_value_from_context(&self, ctx: TypeContext, symbol: Symbol)
        -> Option<&ValueEntry>
    {
        self.type_environments.get(&ctx)
            .and_then(|env| env.get_value(symbol))
    }

    fn get_value_from_current_context(&self, symbol: Symbol)
        -> Option<&ValueEntry>
    {
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
}

impl SemanticContext {
    fn handle_insert_error<T: Debug>(
        &mut self,
        kind: &str,
        context: TypeContext,
        symbol: Symbol,
        result: Option<T>,
    ) -> InsertResult
    {
        if result.is_some() {
            let message = format!(
                "Error inserting '{}' into '{}' environment with context \
                '{:?}'. Value '{:?}' already existed",
                self.get_name_from_symbol(symbol).unwrap(),
                kind,
                context,
                result.unwrap(),
            );
            Err(message)
        } else {
            Ok(())
        }
    }
}
