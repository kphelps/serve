#![feature(trace_macros)]

#[macro_use] extern crate nom;
extern crate tempfile;

mod parser;
mod source_file;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
