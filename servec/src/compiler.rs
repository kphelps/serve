use std::error::Error;
use std::io::{Seek, SeekFrom, Write};
use super::ast;
use super::codegen;
use super::parser;
use super::source_file::SourceFile;
use tempfile;

pub struct Compiler {

}

type CompilationUnit = ast::AST;
type CompilerResult = Result<String, String>;

impl Compiler {

    pub fn new() -> Compiler {
        Compiler {
        }
    }

    pub fn compile(&self, path: &str) -> CompilerResult {
        SourceFile::load(path).map_err(stringify_err)
            .and_then(lex)
            .and_then(parser::parse)
            .and_then(codegen::codegen)
    }

}

fn lex(sf: SourceFile) -> Result<Vec<parser::Token>, String> {
    parser::lex(&sf.body).to_result().map_err(stringify_err)
}

fn stringify_err<E>(e: E) -> String
    where E: Error
{
    e.description().to_string()
}

#[test]
fn test_simple_compile() {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile, "application App end").unwrap();
    tmpfile.seek(SeekFrom::Start(0)).unwrap();

    assert!(Compiler::new().compile(tmpfile.path().to_str().unwrap()).is_ok());
}
