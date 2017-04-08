use super::super::symbol::Symbol;

pub type SemanticResult = Result<ServeType, String>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServeType {
    Unit,
    Int,
    String,
    Record(String, Vec<ServeType>),
    Array(Box<ServeType>),
    Function(Vec<ServeType>, Box<ServeType>),
    Placeholder(Symbol),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueEntry {
    Variable(ServeType),
    Function(Vec<ServeType>, ServeType),
    Endpoint(Vec<ServeType>, ServeType),
}

impl ValueEntry {

    pub fn get_type(&self) -> SemanticResult {
        match *self {
            ValueEntry::Variable(ref tipe) => Ok(tipe.clone()),
            ValueEntry::Function(ref args, ref resp) => {
                Ok(ServeType::Function(args.clone(), Box::new(resp.clone())))
            },
            ValueEntry::Endpoint(ref args, ref resp) => {
                Ok(ServeType::Function(args.clone(), Box::new(resp.clone())))
            },
            ref t => Err(format!("Invalid type '{:?}'", t)),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum TypeContext {
    Global,
    Application,
    Endpoint,
    Function,
    Serializer
}

