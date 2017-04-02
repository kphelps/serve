
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(String, Vec<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
    name: String,
    tipe: String,
}

impl FunctionParameter {
    pub fn new(name: &str, tipe: &str) -> FunctionParameter {
        FunctionParameter {
            name: name.to_owned(),
            tipe: tipe.to_owned(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Application(String, Vec<Statement>),
    Endpoint(String, Vec<FunctionParameter>, String, Vec<Expression>),
    Serializer(String, Vec<Expression>),
    ItemFunctionCall(String, Vec<Expression>),
}

pub type AST = Vec<Statement>;
