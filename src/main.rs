#![feature(trace_macros)]

#[macro_use] extern crate clap;
#[macro_use] extern crate nom;
extern crate tempfile;

mod compiler;
#[macro_use] mod helpers;
mod parser;
mod source_file;

use compiler::Compiler;
use std::env;

fn main() {
    let matches = clap_app!(myapp =>
        (version: "0.0-wayprealpha")
        (author: "Kyle Phelps <kylep91@gmail.com>")
        (about: "Microservice DSL")
        (@arg INPUT: +required ... "Sets the input file to use")
    ).get_matches();

    for path in matches.values_of("INPUT").unwrap() {
        let compiler = Compiler::new();
        println!("{:?}", compiler.compile(path));
    };
}
