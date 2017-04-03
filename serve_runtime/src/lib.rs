extern crate hyper;
extern crate reroute;
extern crate typemap;

#[macro_use] mod exposed_function_registry;
mod app;
pub mod environment;
pub mod types;

pub use app::App;
pub use exposed_function_registry::ExposedFunctionRegistry;
pub use exposed_function_registry::TypeSignature;

pub fn exposed() -> ExposedFunctionRegistry {
    app::exposed()
}
