use libserve::compiler::Compiler;
use super::test_case::{TestCase, TestResult};

pub struct CompileFail {
    path: String
}

impl CompileFail {
    pub fn new(path: String) -> Box<TestCase> {
        Box::new(CompileFail {
            path: path
        })
    }
}

impl TestCase for CompileFail {
    fn name(&self) -> &str {
        &self.path
    }

    fn execute(&self) -> TestResult {
        let compiler = Compiler::new();
        let compiled = compiler.compile(&self.path);
        if compiled.is_err() {
            Ok(())
        } else {
            Err("Expected compilation failure".to_string())
        }
    }
}
