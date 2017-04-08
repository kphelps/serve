#[macro_use] extern crate clap;
extern crate env_logger;
#[macro_use] extern crate log;
extern crate libserve;

use libserve::compiler::Compiler;
use std::env;
use std::process::exit;

fn main() {
    env_logger::init().unwrap();

    let matches = clap_app!(myapp =>
        (version: "0.0-wayprealpha")
        (author: "Kyle Phelps <kylep91@gmail.com>")
        (about: "Microservice DSL")
        (@arg INPUT: +required ... "Sets the input file to use")
    ).get_matches();

    for path in matches.values_of("INPUT").unwrap() {
        let compiler = Compiler::new();
        let compiled = compiler.compile(path);
        if compiled.is_ok() {
            println!("{}", compiled.unwrap());
        } else {
            println!("{}", compiled.unwrap_err());
            exit(1);
        }
    };
}
