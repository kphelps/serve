use std::error::Error;
use std::io::{Seek, SeekFrom, Write};
use super::parser;
use super::source_file::SourceFile;
use tempfile;

pub struct Compiler {

}

type CompilationUnit = Vec<parser::Statement>;
type CompilerResult = Result<CompilationUnit, String>;

impl Compiler {

    pub fn new() -> Compiler {
        Compiler {
        }
    }

    pub fn compile(&self, path: &str) -> CompilerResult {
        SourceFile::load(path)
            .map_err(|e| { e.description().to_string() })
            .and_then(|sf| {
                parser::lex(&sf.body).to_result()
                    .map_err(|e| { e.description().to_string() })
            })
            .and_then(parser::parse)
    }

}

#[test]
fn test_simple_compile() {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile, "application App end").unwrap();
    tmpfile.seek(SeekFrom::Start(0)).unwrap();

    assert!(Compiler::new().compile(tmpfile.path().to_str().unwrap()).is_ok());
}
