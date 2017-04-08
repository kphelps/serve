use super::symbol::Symbol;

pub type Block = Vec<Statement>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConditionalSection {
    predicate: Option<Expression>,
    body: Block,
}

impl ConditionalSection {
    pub fn new(predicate: Option<Expression>, body: Block) -> Self {
        Self {
            predicate: predicate,
            body: body,
        }
    }

    pub fn has_predicate(&self) -> bool {
        self.predicate.is_some()
    }

    pub fn get_predicate(&self) -> &Expression {
        self.predicate.as_ref().unwrap()
    }

    pub fn get_body(&self) -> &Block {
        &self.body
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(Symbol, Vec<Expression>),
    MethodCall(Box<Expression>, Symbol, Vec<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(Symbol),
    Conditional(Vec<ConditionalSection>)
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Let(Symbol, Expression),
    Expression(Expression),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Declaration {
    Function(Symbol, Vec<FunctionParameter>, Symbol, Vec<Statement>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApplicationStatement {
    Endpoint(Symbol, Vec<FunctionParameter>, Symbol, Vec<Statement>),
    ItemFunctionCall(Symbol, Vec<Expression>),
    Declaration(Declaration),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Application(Symbol, Vec<ApplicationStatement>),
    Serializer(Symbol, Vec<Statement>),
    Declaration(Declaration),
}

pub type AST = Vec<TopLevelDeclaration>;
