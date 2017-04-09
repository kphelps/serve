use super::super::symbol::Symbol;

pub type SemanticResult = Result<ServeType, String>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServeType {
    Unit,
    Record(String, Vec<ServeType>),
    Array(Box<ServeType>),
    Function(Vec<ServeType>, Box<ServeType>),
    Placeholder(Symbol),
    Builtin(Symbol),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodHeader {
    uses_self: bool,
    args: Vec<ServeType>,
    return_type: ServeType,
}

impl MethodHeader {
    pub fn new(args: Vec<ServeType>, return_type: ServeType) -> Self {
        Self {
            uses_self: false,
            args: args,
            return_type: return_type,
        }
    }

    pub fn args(&self) -> &Vec<ServeType> {
        &self.args
    }

    pub fn return_type(&self) -> &ServeType {
        &self.return_type
    }
}

