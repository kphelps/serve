use super::ast::*;
use super::symbol::{Symbol, SymbolBindingTable, SymbolRegistry};


#[derive(Debug)]
pub enum ServeType {
    Unit,
    Int,
    String,
    Record(String, Vec<ServeType>),
    Array(Box<ServeType>),
    Placeholder(Symbol),
}

enum ValueEntry {
    Variable(ServeType),
    Function(Vec<ServeType>, ServeType),
}

struct Environment {
    types: SymbolBindingTable<ServeType>,
    values: SymbolBindingTable<ValueEntry>,
}

impl Environment {
    fn new() -> Self {
        Self {
            types: SymbolBindingTable::new(),
            values: SymbolBindingTable::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.types.enter_scope();
        self.values.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.types.leave_scope();
        self.values.leave_scope();
    }
}

pub type SemanticAnalysisResult = Result<ServeType, String>;

struct SemanticAnalysisContext {
    environment: Environment,
    symbols: SymbolRegistry,
}

impl SemanticAnalysisContext {

    pub fn new(symbols: SymbolRegistry) -> Self {
        Self {
            environment: Environment::new(),
            symbols: symbols,
        }
    }

    pub fn type_check_declarations(&mut self, stmts: &Vec<Declaration>) -> SemanticAnalysisResult {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_declaration(stmt)?;
        }
        Ok(result)
    }

    pub fn type_check_declaration(&mut self, stmt: &Declaration) -> SemanticAnalysisResult {
        match *stmt {
            Declaration::Application(ref name, ref body) => {
                self.type_check_application_statements(body)
            },
            Declaration::Serializer(ref name, ref body) => {
                self.with_scope(|ctx| {
                    ctx.type_check_expressions(body)
                })
            },
        }
    }

    pub fn type_check_application_statements(&mut self, stmts: &Vec<ApplicationStatement>)
        -> SemanticAnalysisResult
    {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_application_statement(stmt)?;
        }
        Ok(result)
    }

    pub fn type_check_application_statement(&mut self, stmt: &ApplicationStatement)
        -> SemanticAnalysisResult
    {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref returntype, ref body) => {
                // TODO: expose endpoint in the result?
                self.with_scope(|ctx| {
                    ctx.type_check_expressions(body)
                })
            },
            ApplicationStatement::ItemFunctionCall(ref name, ref args) => {
                // TODO: lookup name in environment and check args against function
                self.type_check_expressions(args)
            }
        }
    }

    pub fn type_check_expressions(&mut self, exprs: &Vec<Expression>)
        -> SemanticAnalysisResult
    {
        let mut result = ServeType::Unit;
        for expr in exprs {
            result = self.type_check_expression(expr)?;
        }
        Ok(result)
    }

    pub fn type_check_expression(&mut self, expr: &Expression)
        -> SemanticAnalysisResult
    {
        match expr {
            _ => Err("Not implemented yet".to_string()),
        }
    }

    fn with_scope<F>(&mut self, f: F) -> SemanticAnalysisResult
        where F: Fn(&mut SemanticAnalysisContext) -> SemanticAnalysisResult
    {
        self.environment.enter_scope();
        let result = f(self);
        self.environment.leave_scope();
        result
    }

}

pub fn type_check(
    ast: AST,
    symbols: SymbolRegistry,
) -> SemanticAnalysisResult {
    let mut ctx = SemanticAnalysisContext::new(symbols);
    ctx.type_check_declarations(&ast)
}
