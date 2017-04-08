pub type TestResult = Result<(), String>;

pub trait TestCase {
    fn name(&self) -> &str;
    fn execute(&self) -> TestResult;
}
