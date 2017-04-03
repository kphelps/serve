use hyper::Server;
use hyper::server::{Request, Response};
use reroute::{Captures, RouterBuilder};
use std::str::FromStr;
use super::environment::{Env, EnvRef};
use super::types::{ServeInt64, ServeString};
use super::exposed_function_registry::ExposedFunctionRegistry;

pub struct App {
    host: String,
    port: u16,
    router_builder: RouterBuilder,
}

impl App {

    pub fn new() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 8080,
            router_builder: RouterBuilder::new(),
        }
    }

    pub fn with_env<F>(&mut self, f: F)
        where F: Fn(&mut Env, &mut App)
    {
        let mut env = Env::new();
        f(&mut env, self);
    }

    pub fn start(self) {
        let router = self.router_builder.finalize().unwrap();
        println!("Running application on {}:{}", self.host, self.port);
        Server::http(format!("{}:{}", self.host, self.port)).unwrap()
            .handle(router).unwrap();
    }

    pub fn host(&mut self, env: &mut Env, new_host: EnvRef<ServeString>) {
        self.host = new_host.lookup(env).value().to_string();
    }

    pub fn port(&mut self, env: &mut Env, new_port: EnvRef<ServeInt64>) {
        self.port = new_port.lookup(env).value().clone() as u16;
    }

    pub fn route<H>(
        &mut self,
        method: &str,
        route: &str,
        handler: H,
    ) where H: Fn(Request, Response, Captures) + Send + Sync + 'static
    {
        let h_method = FromStr::from_str(method).unwrap();
        self.router_builder.route(h_method, route, handler);
    }
}

expose_methods!(application, App,
    start() -> (),
    host(String) -> (),
    port(Int) -> ()
);
