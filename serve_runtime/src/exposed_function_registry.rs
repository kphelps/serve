use std::collections::HashMap;

pub struct ExposedFunctionRegistry {
    local: HashMap<String, TypeSignature>,
    scopes: HashMap<String, Box<ExposedFunctionRegistry>>,
}

#[derive(Clone)]
pub struct TypeSignature {
    pub args: Vec<String>,
    pub return_type: Option<String>,
}

impl ExposedFunctionRegistry {

    pub fn new() -> Self {
        Self {
            local: HashMap::new(),
            scopes: HashMap::new(),
        }
    }

    pub fn register_local(&mut self, name: &str, args: Vec<String>, return_type: &str) {
        let signature = TypeSignature {
            args: args,

            return_type: if return_type == "()" {
                None
            } else {
                Some(return_type.to_string())
            },
        };
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
        pub fn exposed() -> ExposedFunctionRegistry {
            let mut methods = ExposedFunctionRegistry::new();
            $(
                methods.register(
                    stringify!($scope),
                    stringify!($method_name),
                    vec![$(stringify!($args).to_string()),*],
                    stringify!($return),
                );
            )*
            methods
        }
    };
}
