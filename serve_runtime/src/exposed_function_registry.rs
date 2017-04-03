use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExposedType {
    serve_name: String,
    rust_name: String,
    methods: HashMap<String, TypeSignature>,
}

impl ExposedType {
    pub fn new(serve_name: &str, rust_name: &str) -> Self {
        Self {
            serve_name: serve_name.to_string(),
            rust_name: rust_name.to_string(),
            methods: HashMap::new(),
        }
    }

    pub fn get_rust_name<'a>(&'a self) -> &'a str {
        &self.rust_name
    }

    pub fn get_serve_name<'a>(&'a self) -> &'a str {
        &self.serve_name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExposedFunctionRegistry {
    local: HashMap<String, TypeSignature>,
    scopes: HashMap<String, Box<ExposedFunctionRegistry>>,
    types: HashMap<String, ExposedType>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeSignature {
    pub args: Vec<String>,
    pub return_type: Option<String>,
}

impl TypeSignature {
    pub fn new(args: Vec<String>, return_type: &str) -> Self {
        Self {
            args: args,

            return_type: if return_type == "()" {
                None
            } else {
                Some(return_type.to_string())
            },
        }
    }
}

impl ExposedFunctionRegistry {

    pub fn new() -> Self {
        Self {
            local: HashMap::new(),
            scopes: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn register_type(&mut self, tipe: &str, rust_type: &str) {
        self.types.entry(tipe.to_string())
            .or_insert_with(|| ExposedType::new(tipe, rust_type));
    }

    pub fn register_type_method(
        &mut self,
        tipe: &str,
        rust_type: &str,
        name: &str,
        args: Vec<String>,
        return_type: &str,
    ) {
        let signature = TypeSignature::new(args, return_type);
        self.types.entry(tipe.to_string())
            .or_insert_with(|| ExposedType::new(tipe, rust_type))
            .methods
            .insert(name.to_string(), signature);
    }

    pub fn lookup_type<'a>(&'a self, tipe: &str) -> Option<&'a ExposedType> {
        self.types.get(tipe)
    }

    pub fn register_local(&mut self, name: &str, args: Vec<String>, return_type: &str) {
        let signature = TypeSignature::new(args, return_type);
        self.local.insert(name.to_string(), signature);
    }

    pub fn register(&mut self, scope: &str, name: &str, args: Vec<String>, return_type: &str) {
        self.scopes.entry(scope.to_string())
            .or_insert_with(|| Box::new(ExposedFunctionRegistry::new()))
            .register_local(name, args, return_type)
    }

    pub fn lookup_local<'a>(&'a self, name: &str) -> Option<&'a TypeSignature> {
        self.local.get(name)
    }

    pub fn lookup_scope<'a>(&'a self, scope: &str, name: &str) -> Option<&'a TypeSignature> {
        self.scopes.get(scope).and_then(|r| r.lookup_local(name))
    }
}

#[macro_export]
macro_rules! expose_methods {
    ($scope:ident, $rust_struct:ident, $( $method_name:ident ( $($args:ty),* ) -> $return:ty ),* ) => {
        pub fn expose(registry: &mut ExposedFunctionRegistry) {
            $(
                registry.register(
                    stringify!($scope),
                    stringify!($method_name),
                    vec![$(stringify!($args).to_string()),*],
                    stringify!($return),
                );
            )*
        }
    };
}

#[macro_export]
macro_rules! expose_type_methods {
    (
        $serve_type:ident,
        $rust_type:ty,
        $( $method_name:ident ( $($args:ty),* ) -> $return:ty ),*
    ) => {
        pub fn expose(registry: &mut $crate::ExposedFunctionRegistry) {
            registry.register_type(
                stringify!($serve_type),
                stringify!($rust_type)
            );
            $(
                registry.register_type_method(
                    stringify!($serve_type),
                    stringify!($rust_type),
                    stringify!($method_name),
                    vec![$(stringify!($args).to_string()),*],
                    stringify!($return),
                );
            )*
        }
    };
}
