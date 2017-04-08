use libserve::compiler::Compiler;
use super::test_case::{TestCase, TestResult};

pub struct CompilePass {
    path: String
}

impl CompilePass {
    pub fn new(path: String) -> Box<TestCase> {
        Box::new(CompilePass {
            path: path
        })
    }
}

impl TestCase for CompilePass {
    fn name(&self) -> &str {
        &self.path
    }

    fn execute(&self) -> TestResult {
        let compiler = Compiler::new();
        compiler.compile(&self.path)
            .and(Ok(()))
    }
}
