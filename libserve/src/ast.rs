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

    pub fn get_name(&self) -> Symbol {
        self.name
    }

    pub fn get_type(&self) -> Symbol {
        self.tipe
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Function(Symbol, Vec<FunctionParameter>, Symbol, Vec<Expression>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ApplicationStatement {
    Endpoint(Symbol, Vec<FunctionParameter>, Symbol, Vec<Expression>),
    ItemFunctionCall(Symbol, Vec<Expression>),
    Statement(Statement),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Declaration {
    Application(Symbol, Vec<ApplicationStatement>),
    Serializer(Symbol, Vec<Expression>),
    Statement(Statement),
}

pub type AST = Vec<Declaration>;
