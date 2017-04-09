use super::context::SemanticContext;
use super::super::ast::*;
use super::super::symbol::Symbol;
use super::types::{SemanticResult, ServeType, TypeContext, ValueEntry};
use super::type_registrar::TypeRegistrar;


pub trait TypeChecker {
    fn type_check_top_level_declarations(&mut self, decls: &Vec<TopLevelDeclaration>)
        -> SemanticResult;
}

impl TypeChecker for SemanticContext {

    fn type_check_top_level_declarations(&mut self, decls: &Vec<TopLevelDeclaration>) -> SemanticResult {
        let mut result = ServeType::Unit;
        for decl in decls {
            result = self.type_check_top_level_declaration(decl)?;
        }
        Ok(result)
    }
}

impl SemanticContext {
    fn type_check_top_level_declaration(&mut self, decl: &TopLevelDeclaration) -> SemanticResult {
        match *decl {
            TopLevelDeclaration::Application(ref name, ref body) => {
                self.with_context_scope(TypeContext::Application, |ctx| {
                    ctx.type_check_application_statements(body)
                })
            },
            TopLevelDeclaration::Serializer(ref name, ref body) => {
                self.with_context_scope(TypeContext::Serializer, |ctx| {
                    ctx.type_check_statements(body)
                })
            },
            TopLevelDeclaration::Declaration(ref stmt) => {
                self.type_check_declaration(stmt)
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
                // TODO: check if action exists
                self.type_check_function_like_decl(
                    TypeContext::Endpoint,
                    name,
                    args,
                    return_type_name,
                    body
                )
            },
            ApplicationStatement::ItemFunctionCall(ref name, ref args) => {
                self.type_check_function_call(name, args)
            }
            ApplicationStatement::Declaration(ref stmt) => {
                self.type_check_declaration(stmt)
            }
        }
    }

    fn type_check_declaration(&mut self, stmt: &Declaration)
        -> SemanticResult
    {
        match *stmt {
            Declaration::Function(ref name, ref args, ref return_type_name, ref body) => {
                self.type_check_function_like_decl(
                    TypeContext::Function,
                    name,
                    args,
                    return_type_name,
                    body
                )
            }
        }
    }

    fn type_check_statements(&mut self, stmts: &Vec<Statement>)
        -> SemanticResult
    {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_statement(stmt)?;
        }
        Ok(result)
    }

    fn type_check_statement(&mut self, stmt: &Statement) -> SemanticResult {
        match *stmt {
            Statement::Let(ref name, ref body) => {
                let body_type = self.type_check_expression(body)?;
                self.register_value(*name, ValueEntry::Variable(body_type))?;
                Ok(ServeType::Unit)
            },
            Statement::Expression(ref inner) => {
                self.type_check_expression(inner)
            },
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
                self.resolve_builtin_type("Int")
            },
            Expression::StringLiteral(_) => {
                self.resolve_builtin_type("String")
            },
            Expression::UnitLiteral => {
                Ok(ServeType::Unit)
            },
            Expression::Identifier(ref name) => {
                self.get_value(*name)
                    .ok_or(format!("Undeclared identifier '{}'", self.symbols.get_name(name).unwrap()))
                    .and_then(|v| ValueEntry::get_type(&v))
            },
            Expression::Conditional(ref sections) => {
                // type check predicates?
                let ty = self.with_scope(|ctx| {
                    ctx.type_check_statements(sections[0].get_body())
                })?;
                for section in &sections[1..] {
                    let section_type = self.with_scope(|ctx| {
                        ctx.type_check_statements(section.get_body())
                    })?;
                    self.check_types(&ty, section_type)?;
                }
                Ok(ty)
            },
            Expression::Assignment(ref lhs, ref rhs) => {
                let lhs_type = self.type_check_expression(lhs)?;
                let rhs_type = self.type_check_expression(rhs)?;
                self.check_types(&lhs_type, rhs_type)
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
                    // hmm... Variable(Function) vs Function?
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
                    ValueEntry::Function(ref fn_arg_types, ref fn_return) => {
                        if fn_arg_types == &call_arg_types {
                            Ok(fn_return.clone())
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

    fn type_check_function_like_decl(
        &mut self,
        scope: TypeContext,
        name: &Symbol,
        args: &Vec<FunctionParameter>,
        return_type_name: &Symbol,
        body: &Vec<Statement>
    ) -> SemanticResult
    {
        let actual_return = self.with_context_scope(scope, |ctx| {
            for arg in args {
                let arg_type = ctx.get_type(arg.get_type());
                ctx.register_value(
                    arg.get_name(),
                    ValueEntry::Variable(arg_type)
                );
            }
            ctx.type_check_statements(body)
        })?;
        let return_type = self.get_type(*return_type_name);
        self.check_types(&return_type, actual_return)?;
        Ok(ServeType::Unit)
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
