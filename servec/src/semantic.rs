use std::collections::HashMap;
use super::ast::*;


type Symbol = usize;

struct SymbolRegistry {
    symbolsToName: HashMap<Symbol, String>,
    namesToSymbol: HashMap<String, Symbol>,
}

impl SymbolRegistry {
    pub fn new() -> SymbolRegistry {
        Self {
            symbolsToName: HashMap::new(),
            namesToSymbol: HashMap::new(),
        }
    }

    pub fn get_symbol<S>(&mut self, name: S) -> Symbol
        where S: Into<String>
    {
        let cur_len = self.namesToSymbol.len();
        let name_string = name.into();
        let symbol = self.namesToSymbol.entry(name_string.clone()).or_insert(cur_len);
        self.symbolsToName.entry(*symbol).or_insert(name_string);
        *symbol
    }

    pub fn get_name(&mut self, symbol: &Symbol) -> Option<String> {
        self.symbolsToName.get(symbol).cloned()
    }
}

struct SymbolBindingTable<T> {
    values: Vec<HashMap<Symbol, T>>
}

impl<T> SymbolBindingTable<T> {

    pub fn new() -> Self {
        Self {
            values: vec![HashMap::new()]
        }
    }

    pub fn insert(&mut self, symbol: Symbol, value: T) {
        self.current_scope().insert(symbol, value);
    }

    pub fn get(&self, symbol: Symbol) -> Option<&T> {
        for scope in self.values.iter().rev() {
            if scope.contains_key(&symbol) {
                return Some(&scope[&symbol]);
            }
        }
        None
    }

    pub fn enter_scope(&mut self) {
        self.values.push(HashMap::new())
    }

    pub fn leave_scope(&mut self) {
        self.values.pop();
    }

    fn current_scope(&mut self) -> &mut HashMap<Symbol, T> {
        self.values.last_mut().unwrap()
    }
}

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
}

impl SemanticAnalysisContext {

    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    pub fn type_check_statements(&mut self, stmts: &Vec<Statement>) -> SemanticAnalysisResult {
        let mut result = ServeType::Unit;
        for stmt in stmts {
            result = self.type_check_statement(stmt)?;
        }
        Ok(result)
    }

    pub fn type_check_statement(&mut self, stmt: &Statement) -> SemanticAnalysisResult {
        match *stmt {
            Statement::Application(ref name, ref body) => {
                self.type_check_statements(body)
            },
            Statement::Endpoint(ref name, ref args, ref returntype, ref body) => {
                // TODO: expose endpoint in the result?
                self.with_scope(|ctx| {

                    ctx.type_check_expressions(body)
                })
            },
            Statement::Serializer(ref name, ref body) => {
                self.with_scope(|ctx| {
                    ctx.type_check_expressions(body)
                })
            },
            Statement::ItemFunctionCall(ref name, ref args) => {
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

pub fn type_check(ast: AST) -> SemanticAnalysisResult {
    let mut ctx = SemanticAnalysisContext::new();
    ctx.type_check_statements(&ast)
}
