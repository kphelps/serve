use super::context::SemanticContext;
use super::super::ast::*;
use super::super::symbol::Symbol;
use super::types::{ServeType};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IRFragment {
    args: Vec<ServeType>,
    return_type: ServeType,
    body: IRStatement,
}

impl IRFragment {
    pub fn new(
        args: Vec<ServeType>,
        return_type: ServeType,
        body: IRStatement,
    ) -> Self {
        Self {
            args: args,
            return_type: return_type,
            body: body,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IRStatement {
    Let(Symbol, IRExpression),
    Expression(IRExpression),
    Return(IRExpression),
    Seq(Box<IRStatement>, Box<IRStatement>),
    NoOp,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IRExpression {
    Call(Symbol, Vec<IRExpression>),
    MethodCall(ServeType, Symbol, Vec<IRExpression>),
    Move(Box<IRExpression>, Box<IRExpression>),
    BoolLiteral(bool),
    IntLiteral(i64),
    StringLiteral(String),
    UnitLiteral,
    Conditional(Box<IRExpression>, Box<IRStatement>, Box<IRStatement>),
    Eseq(Vec<IRExpression>, Symbol),
    Temp(Symbol),
    Identifier(Symbol),
}

pub trait IRContext {

    fn transform_unit(&mut self)
        -> IRExpression;

    fn transform_int(&mut self, value: i64)
        -> IRExpression;

    fn transform_string(&mut self, value: &str)
        -> IRExpression;

    fn transform_identifier(&mut self, name: &Symbol)
        -> IRExpression;

    fn transform_return(&mut self, expr: IRExpression)
        -> IRStatement;

    fn transform_expression_to_statement(&mut self, expr: IRExpression)
        -> IRStatement;

    fn transform_conditional(
        &mut self,
        parts: Vec<(Option<IRExpression>, IRStatement)>
    ) -> IRExpression;

    fn transform_assignment(&mut self, lhs: IRExpression, rhs: IRExpression)
        -> IRExpression;

    fn transform_method_call(
        &mut self,
        receiver_type: ServeType,
        receiver: IRExpression,
        name: &Symbol,
        args: Vec<IRExpression>
    ) -> IRExpression;

    fn transform_call(
        &mut self,
        name: &Symbol,
        args: Vec<IRExpression>
    ) -> IRExpression;

    fn transform_let(
        &mut self,
        name: &Symbol,
        body: IRExpression,
    ) -> IRStatement;

    fn transform_function_decl(
        &mut self,
        name: &Symbol,
        args: &Vec<ServeType>,
        return_type: &ServeType,
        body: IRStatement,
    ) -> IRStatement;

    fn register_fragment(&mut self, name: &Symbol, fragment: IRFragment);

    fn sequence_statements(&mut self, stmts: Vec<IRStatement>)
        -> IRStatement;

    fn sequence_expressions(&mut self, exprs: Vec<IRExpression>)
        -> IRStatement;
}

impl IRContext for SemanticContext {

    fn transform_unit(&mut self) -> IRExpression {
        IRExpression::UnitLiteral
    }

    fn transform_int(&mut self, value: i64) -> IRExpression {
        IRExpression::IntLiteral(value)
    }

    fn transform_string(&mut self, value: &str) -> IRExpression {
        IRExpression::StringLiteral(value.to_string())
    }

    fn transform_identifier(&mut self, name: &Symbol) -> IRExpression {
        IRExpression::Identifier(*name)
    }

    fn transform_expression_to_statement(&mut self, expr: IRExpression)
        -> IRStatement
    {
        IRStatement::Expression(expr)
    }

    fn transform_return(&mut self, expr: IRExpression) -> IRStatement {
        IRStatement::Return(expr)
    }

    fn transform_conditional(
        &mut self,
        parts: Vec<(Option<IRExpression>, IRStatement)>
    ) -> IRExpression
    {
        let (last_pred, last_stmt) = parts.last().unwrap().clone();
        let mut last_clause = if last_pred.is_some() {
            let temp = self.get_temp();
            IRExpression::Conditional(
                Box::new(last_pred.unwrap()),
                Box::new(last_stmt),
                Box::new(IRStatement::Expression(IRExpression::UnitLiteral))
            )
        } else {
            IRExpression::Conditional(
                Box::new(IRExpression::BoolLiteral(true)),
                Box::new(last_stmt),
                Box::new(IRStatement::NoOp)
            )
        };
        for (pred, stmt) in parts.into_iter().rev().skip(1) {
            last_clause = IRExpression::Conditional(
                Box::new(pred.unwrap()),
                Box::new(stmt),
                Box::new(IRStatement::Expression(last_clause))
            );
        }
        last_clause
    }

    fn transform_assignment(&mut self, lhs: IRExpression, rhs: IRExpression)
        -> IRExpression
    {
        IRExpression::Move(Box::new(lhs), Box::new(rhs))
    }

    fn transform_method_call(
        &mut self,
        receiver_type: ServeType,
        receiver: IRExpression,
        name: &Symbol,
        mut args: Vec<IRExpression>
    ) -> IRExpression
    {
        args.insert(0, receiver);
        // TODO: should propbably have method resolution happen by now.
        IRExpression::MethodCall(receiver_type, *name, args)
    }

    fn transform_call(
        &mut self,
        name: &Symbol,
        args: Vec<IRExpression>
    ) -> IRExpression
    {
        IRExpression::Call(*name, args)
    }

    fn transform_let(
        &mut self,
        name: &Symbol,
        body: IRExpression,
    ) -> IRStatement
    {
        IRStatement::Let(*name, body)
    }

    fn transform_function_decl(
        &mut self,
        name: &Symbol,
        args: &Vec<ServeType>,
        return_type: &ServeType,
        body: IRStatement,
    ) -> IRStatement
    {
        let fragment = IRFragment::new(args.clone(), return_type.clone(), body);
        self.register_fragment(name, fragment);
        IRStatement::NoOp
    }

    fn register_fragment(&mut self, name: &Symbol, fragment: IRFragment)
    {
        let result = self.fragments.insert(*name, fragment);
        assert!(result.is_none());
    }

    fn sequence_statements(&mut self, stmts: Vec<IRStatement>)
        -> IRStatement
    {
        if stmts.len() == 0 {
            return IRStatement::NoOp;
        }
        let mut result = stmts.last().unwrap().clone();
        for stmt in stmts.into_iter().rev().skip(1) {
            result = IRStatement::Seq(Box::new(stmt), Box::new(result));
        }
        result
    }

    fn sequence_expressions(&mut self, exprs: Vec<IRExpression>)
        -> IRStatement
    {
        let stmts = exprs.into_iter().map(IRStatement::Expression).collect();
        self.sequence_statements(stmts)
    }
}


impl SemanticContext {
    fn get_temp(&mut self) -> IRExpression {
        IRExpression::Temp(self.symbols.get_temp())
    }
}

