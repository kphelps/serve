use super::context::SemanticContext;
use super::super::ast::*;
use super::super::symbol::Symbol;
use super::ir::*;
use super::types::{SemanticResult, ServeType, TypeContext, ValueEntry};
use super::type_registrar::TypeRegistrar;

type TypeResult<T> = Result<(T, ServeType), String>;

pub trait TypeChecker {
    fn type_check_top_level_declarations(
        &mut self,
        decls: &Vec<TopLevelDeclaration>
    ) -> TypeResult<IRStatement>;
}

impl TypeChecker for SemanticContext {

    fn type_check_top_level_declarations(
        &mut self,
        decls: &Vec<TopLevelDeclaration>
    ) -> TypeResult<IRStatement>
    {
        let mut result = ServeType::Unit;
        let mut ir_stmts = Vec::new();
        for decl in decls {
            let (ir_stmt, result) = self.type_check_top_level_declaration(decl)?;
            ir_stmts.push(ir_stmt);
        }
        Ok((self.sequence_statements(ir_stmts), result))
    }
}

impl SemanticContext {
    fn type_check_top_level_declaration(
        &mut self,
        decl: &TopLevelDeclaration
    ) -> TypeResult<IRStatement>
    {
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
        -> TypeResult<IRStatement>
    {
        let mut result = ServeType::Unit;
        let mut ir_stmts = Vec::new();
        for stmt in stmts {
            let (ir_stmt, result) = self.type_check_application_statement(stmt)?;
            ir_stmts.push(ir_stmt);
        }
        Ok((self.sequence_statements(ir_stmts), result))
    }

    fn type_check_application_statement(&mut self, stmt: &ApplicationStatement)
        -> TypeResult<IRStatement>
    {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref return_type_name, ref body) => {
                // TODO: expose endpoint in the result?
                // TODO: check if action exists
                // TODO: transform endpoint registration into IR
                // TODO: Expand serializer based on types
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
                    .map(|(ir, tipe)| (self.transform_expression_to_statement(ir), tipe))
            },
            ApplicationStatement::Declaration(ref stmt) => {
                self.type_check_declaration(stmt)
            },
        }
    }

    fn type_check_declaration(&mut self, stmt: &Declaration)
        -> TypeResult<IRStatement>
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
        -> TypeResult<IRStatement>
    {
        let mut result = ServeType::Unit;
        let mut ir_stmts = Vec::new();
        for stmt in stmts {
            let (ir_stmt, stmt_result) = self.type_check_statement(stmt)?;
            ir_stmts.push(ir_stmt);
            result = stmt_result;
        }
        Ok((self.sequence_statements(ir_stmts), result))
    }

    fn type_check_statement(&mut self, stmt: &Statement)
        -> TypeResult<IRStatement>
    {
        match *stmt {
            Statement::Let(ref name, ref body) => {
                let (body_ir, body_type) = self.type_check_expression(body)?;
                self.register_value(*name, ValueEntry::Variable(body_type))?;
                Ok((self.transform_let(name, body_ir), ServeType::Unit))
            },
            Statement::Return(ref return_expr) => {
                let (child, tipe) = self.type_check_expression(return_expr)?;
                Ok((self.transform_return(child), tipe))
            },
            Statement::Expression(ref inner) => {
                self.type_check_expression(inner)
                    .map(|(ir, tipe)| (self.transform_expression_to_statement(ir), tipe))
            },
        }
    }

    fn type_check_expressions(&mut self, exprs: &Vec<Expression>)
        -> TypeResult<IRStatement>
    {
        let mut result = ServeType::Unit;
        let mut ir_exprs = Vec::new();
        for expr in exprs {
            let (ir_expr, result) = self.type_check_expression(expr)?;
            ir_exprs.push(ir_expr);
        }
        Ok((self.sequence_expressions(ir_exprs), result))
    }

    fn type_check_expression(&mut self, expr: &Expression)
        -> TypeResult<IRExpression>
    {
        match *expr {
            Expression::FunctionCall(ref name, ref args) => {
                self.type_check_function_call(name, args)
            },
            Expression::IntLiteral(ref value) => {
                let tipe = self.resolve_builtin_type("Int")?;
                Ok((self.transform_int(*value), tipe))
            },
            Expression::StringLiteral(ref value) => {
                let tipe = self.resolve_builtin_type("String")?;
                Ok((self.transform_string(value), tipe))
            },
            Expression::UnitLiteral => {
                Ok((self.transform_unit(), ServeType::Unit))
            },
            Expression::Identifier(ref name) => {
                let ir_id = self.transform_identifier(name);
                self.get_value(*name)
                    .ok_or(format!("Undeclared identifier '{}'", self.symbols.get_name(name).unwrap()))
                    .and_then(|v| ValueEntry::get_type(&v))
                    .map(|t| (ir_id, t))
            },
            Expression::Conditional(ref sections) => {
                let (pred0, _) = self.type_check_expression(sections[0].get_predicate())?;
                let (stmt0, ty) = self.with_scope(|ctx| {
                    ctx.type_check_statements(sections[0].get_body())
                })?;
                let mut parts = vec![(Some(pred0), stmt0)];
                for section in &sections[1..] {
                    let ir_pred = if section.has_predicate() {
                        let (ir, _) = self.type_check_expression(section.get_predicate())?;
                        Some(ir)
                    } else {
                        None
                    };
                    let (ir_stmt, section_type) = self.with_scope(|ctx| {
                        ctx.type_check_statements(section.get_body())
                    })?;
                    self.check_types(&ty, section_type)?;
                    parts.push((ir_pred, ir_stmt));
                }
                Ok((self.transform_conditional(parts), ty))
            },
            Expression::Assignment(ref lhs, ref rhs) => {
                let (lhs_ir, lhs_type) = self.type_check_expression(lhs)?;
                let (rhs_ir, rhs_type) = self.type_check_expression(rhs)?;
                Ok((
                    self.transform_assignment(lhs_ir, rhs_ir),
                    self.check_types(&lhs_type, rhs_type)?
                ))
            },
            Expression::MethodCall(ref receiver, ref name, ref args) => {
                let (ir_receiver, receiver_type) = self.type_check_expression(receiver)?;
                let method_header = self.resolve_method(&receiver_type, name)?.clone();
                let (ir_args, _) = self.type_check_args(method_header.args(), args)?;
                Ok((
                    self.transform_method_call(receiver_type, ir_receiver, name, ir_args),
                    method_header.return_type().clone()
                ))
            },
            _ => Err("Not implemented yet".to_string()),
        }
    }

    fn type_check_args(&mut self, expected: &Vec<ServeType>, actual: &Vec<Expression>)
        -> Result<(Vec<IRExpression>, Vec<ServeType>), String>
    {
        let mut ir = Vec::new();
        let mut actual_types = Vec::new();
        for e in actual {
            let (e_ir, e_type) = self.type_check_expression(e)?;
            ir.push(e_ir);
            actual_types.push(e_type);
        }
        if &actual_types == expected {
            Ok((ir, actual_types))
        } else {
            Err(format!("Invalid args '{:?}'. Expected types of '{:?}'", actual, expected))
        }
    }

    fn type_check_function_call(&mut self, name: &Symbol, args: &Vec<Expression>)
        -> TypeResult<IRExpression>
    {
        let fn_val = self.get_value(*name)
            .ok_or(format!("Undeclared function '{}'", self.symbols.get_name(name).unwrap()))?;
        match fn_val {
            // hmm... Variable(Function) vs Function?
            ValueEntry::Variable(ServeType::Function(ref fn_arg_types, ref fn_return)) => {
                let (args_ir, arg_types) = self.type_check_args(fn_arg_types, args)?;
                Ok((self.transform_call(name, args_ir), *fn_return.clone()))
            },
            ValueEntry::Function(ref fn_arg_types, ref fn_return) => {
                let (args_ir, arg_types) = self.type_check_args(fn_arg_types, args)?;
                Ok((self.transform_call(name, args_ir), fn_return.clone()))
            },
            ref other => Err(format!("Expected callable, found '{:?}'", other)),
        }
    }

    fn type_check_function_like_decl(
        &mut self,
        scope: TypeContext,
        name: &Symbol,
        args: &Vec<FunctionParameter>,
        return_type_name: &Symbol,
        body: &Vec<Statement>
    ) -> TypeResult<IRStatement>
    {
        let mut arg_types = Vec::new();
        let (body_ir, actual_return) = self.with_context_scope(scope, |ctx| {
            for arg in args {
                let arg_type = ctx.get_type(arg.get_type());
                arg_types.push(arg_type.clone());
                ctx.register_value(
                    arg.get_name(),
                    ValueEntry::Variable(arg_type)
                );
            }
            ctx.type_check_statements(body)
        })?;
        let return_type = self.get_type(*return_type_name);
        self.check_types(&return_type, actual_return)?;
        Ok((
            self.transform_function_decl(
                name,
                &arg_types,
                &return_type,
                body_ir
            ),
            ServeType::Unit
        ))
    }

    fn check_types(&mut self, expected: &ServeType, actual: ServeType)
        -> SemanticResult
    {
        if expected == &actual {
            Ok(actual)
        } else {
            let out = Err(format!("Expected '{:?}', found '{:?}'", expected, actual));
            out
        }
    }
}
