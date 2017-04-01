extern crate hyper;
extern crate reroute;

//use hyper::Server;
//use hyper::server::{Request, Response};
//use reroute::{Captures, RouterBuilder};

//fn index_handler(_: Request, res: Response, c: Captures) {
    //println!("captures: {:?}", c);
    //res.send(b"Hello World!").unwrap();
//}

//fn main() {
    //let mut builder = RouterBuilder::new();

    //builder.get(r"/", digit_handler);

    //let router = builder.finalize().unwrap();

    //Server::http("127.0.0.1:3000").unwrap().handle(router).unwrap();
//}

pub struct App {
    host: String,
    port: u16,
}

impl App {

    pub fn new() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 8080,
        }
    }

    pub fn host(&mut self, new_host: String) {
        self.host = new_host;
    }

    pub fn port(&mut self, new_port: i64) {
        self.port = new_port as u16;
    }
}
