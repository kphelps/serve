use super::ExposedFunctionRegistry;
use super::environment::{Env, EnvRef};

pub trait ServeType {
}

pub trait Constructible {
    type Inner;

    fn new<T>(inner: T) -> Self
        where T: Into<Self::Inner>;
}

macro_rules! servify_name {
    ($name:ident) => { serve_$name }
}

macro_rules! serve_fn {
    ($name:ident($env:ident, $($arg:ident: $arg_type:ty),*) -> $return_type:ty $body:block) => {
        fn $name($env: &mut Env, $($arg: EnvRef<$arg_type>),*) -> EnvRef<$return_type> {
            $body
        }
    }
}

macro_rules! wrap_rust_type {
    ($name:ident, $rt_name:ident, $rust_type:ty) => {
        wrap_rust_type!($name, $rt_name, $rust_type,);
    };
    (
        $name:ident,
        $runtime_name:ident,
        $rust_type:ty,
        $($exposed_methods:ident($($args:ty),*) -> $return:ty),*
    ) => {
        #[derive(Debug, Eq, PartialEq)]
        pub struct $runtime_name {
            inner: $rust_type,
        }

        impl $runtime_name {
            pub fn name() -> &'static str {
                stringify!($name)
            }

            pub fn value(&self) -> &$rust_type {
                &self.inner
            }

            expose_type_methods!(
                $name,
                serve_runtime::types::$runtime_name,
                $($exposed_methods($($args),*) -> $return),*
            );
        }

        impl Constructible for $runtime_name {
            type Inner = $rust_type;

            fn new<T>(inner: T) -> Self
                where T: Into<Self::Inner>
            {
                Self {
                    inner: inner.into(),
                }
            }
        }

        impl ServeType for $runtime_name {
        }

    }
}

wrap_rust_type!(String, ServeString, String,
    to_bytes() -> String,
    length() -> Int
);

serve_fn!(string_to_bytes(env, s: ServeString) -> ServeString {
    println!("{:?}", s);
    s
});

serve_fn!(length(env, s: ServeString) -> ServeInt64 {
    let value = s.lookup(env).inner.len() as i64;
    env.allocate(value)
});

wrap_rust_type!(Int, ServeInt64, i64);

pub fn expose(registry: &mut ExposedFunctionRegistry) {
    ServeString::expose(registry);
    ServeInt64::expose(registry);
}
