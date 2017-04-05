use std::collections::HashMap;

pub type Symbol = usize;

pub struct SymbolRegistry {
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

pub struct SymbolBindingTable<T> {
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
