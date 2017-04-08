#![feature(trace_macros)]
#![feature(slice_patterns)]

#![cfg_attr(feature = "nightly", feature(rustc_private))]
extern crate aster;
#[macro_use] extern crate itertools;
#[macro_use] extern crate log;
#[macro_use] extern crate nom;
extern crate serve_runtime;
extern crate tempfile;
#[cfg(feature = "nightly")]
extern crate syntax;
#[cfg(not(feature = "nightly"))]
extern crate syntex_syntax as syntax;

pub mod ast;
//mod codegen;
pub mod compiler;
#[macro_use] pub mod helpers;
pub mod parser;
pub mod semantic;
pub mod source_file;
pub mod static_environment;
pub mod symbol;
