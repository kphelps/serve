use super::context::SemanticContext;
use super::super::ast::*;
use super::super::symbol::Symbol;
use super::types::{SemanticResult, ServeType, TypeContext, ValueEntry};
use super::type_registrar::TypeRegistrar;


pub trait TypeChecker {
    fn type_check_declarations(&mut self, decls: &Vec<Declaration>)
        -> SemanticResult;
    fn type_check_declaration(&mut self, decl: &Declaration)
        -> SemanticResult;
    fn type_check_application_statements(&mut self, decl: &Vec<ApplicationStatement>)
        -> SemanticResult;
    fn type_check_application_statement(&mut self, decl: &ApplicationStatement)
        -> SemanticResult;
    fn type_check_expressions(&mut self, decl: &Vec<Expression>)
        -> SemanticResult;
    fn type_check_expression(&mut self, decl: &Expression)
        -> SemanticResult;

    fn type_check_function_call(&mut self, name: &Symbol, args: &Vec<Expression>)
        -> SemanticResult;
    fn check_types(&mut self, expected: &ServeType, actual: ServeType)
        -> SemanticResult;
}

impl TypeChecker for SemanticContext {

    fn type_check_declarations(&mut self, decls: &Vec<Declaration>) -> SemanticResult {
        let mut result = ServeType::Unit;
        for decl in decls {
            result = self.type_check_declaration(decl)?;
        }
        Ok(result)
    }

    fn type_check_declaration(&mut self, decl: &Declaration) -> SemanticResult {
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

    fn type_check_application_statements(&mut self, stmts: &Vec<ApplicationStatement>)
        -> SemanticResult
    {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_application_statement(stmt)?;
        }
        Ok(result)
    }

    fn type_check_application_statement(&mut self, stmt: &ApplicationStatement)
        -> SemanticResult
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

    fn type_check_expressions(&mut self, exprs: &Vec<Expression>)
        -> SemanticResult
    {
        let mut result = ServeType::Unit;
        for expr in exprs {
            result = self.type_check_expression(expr)?;
        }
        Ok(result)
    }

    fn type_check_expression(&mut self, expr: &Expression)
        -> SemanticResult
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
        -> SemanticResult
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
        -> SemanticResult
    {
        if expected == &actual {
            Ok(actual)
        } else {
            Err(format!("Expected '{:?}', found '{:?}'", expected, actual))
        }
    }
}
