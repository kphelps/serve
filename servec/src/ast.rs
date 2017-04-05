use super::symbol::Symbol;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(Symbol, Vec<Expression>),
    MethodCall(Box<Expression>, Symbol, Vec<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(Symbol),
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
    name: Symbol,
    tipe: Symbol,
}

impl FunctionParameter {
    pub fn new(name: Symbol, tipe: Symbol) -> FunctionParameter {
        FunctionParameter {
            name: name,
            tipe: tipe,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Application(Symbol, Vec<Statement>),
    Endpoint(Symbol, Vec<FunctionParameter>, Symbol, Vec<Expression>),
    Serializer(Symbol, Vec<Expression>),
    ItemFunctionCall(Symbol, Vec<Expression>),
}

pub type AST = Vec<Statement>;
