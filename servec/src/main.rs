#![feature(trace_macros)]

#![cfg_attr(feature = "nightly", feature(rustc_private))]
extern crate aster;
#[macro_use] extern crate clap;
#[macro_use] extern crate itertools;
#[macro_use] extern crate log;
#[macro_use] extern crate nom;
extern crate tempfile;
#[cfg(feature = "nightly")]
extern crate syntax;
#[cfg(not(feature = "nightly"))]
extern crate syntex_syntax as syntax;

mod ast;
mod codegen;
mod compiler;
#[macro_use] mod helpers;
mod parser;
mod source_file;
mod static_environment;

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
        let compiled = compiler.compile(path);
        if compiled.is_ok() {
            println!("{}", compiled.unwrap());
        } else {
            println!("{}", compiled.unwrap_err());
        }
    };
}
