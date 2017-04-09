use super::types::{ServeType, ValueEntry};
use super::super::symbol::{Symbol, SymbolBindingTable, SymbolRegistry};

#[derive(Debug)]
pub struct Environment {
    types: SymbolBindingTable<ServeType>,
    values: SymbolBindingTable<ValueEntry>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            types: SymbolBindingTable::new(),
            values: SymbolBindingTable::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.types.enter_scope();
        self.values.enter_scope();
    }

    pub fn leave_scope(&mut self) {
        self.types.leave_scope();
        self.values.leave_scope();
    }

    pub fn get_value(&self, symbol: Symbol) -> Option<&ValueEntry> {
        self.values.get(symbol)
    }

    pub fn insert_value(&mut self, symbol: Symbol, entry: ValueEntry) -> Option<ValueEntry> {
        self.values.insert(symbol, entry)
    }

    pub fn get_type(&self, symbol: Symbol) -> Option<&ServeType> {
        self.types.get(symbol)
    }

    pub fn insert_type(&mut self, symbol: Symbol, entry: ServeType) -> Option<ServeType> {
        self.types.insert(symbol, entry)
    }
}
