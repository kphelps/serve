extern crate hyper;
extern crate reroute;

use hyper::Server;
use hyper::server::{Request, Response};
use reroute::{Captures, RouterBuilder};
use std::str::FromStr;

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

    pub fn start(self) {
        let router = self.router_builder.finalize().unwrap();
        Server::http(format!("{}:{}", self.host, self.port)).unwrap()
            .handle(router).unwrap();
    }

    pub fn host(&mut self, new_host: &str) {
        self.host = new_host.to_string();
    }

    pub fn port(&mut self, new_port: i64) {
        self.port = new_port as u16;
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
