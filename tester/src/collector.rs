use std::fs::read_dir;
use std::path::Path;
use super::test_case::TestCase;
use super::compile_pass::CompilePass;
use super::compile_fail::CompileFail;

type TestCaseCollection = Vec<Box<TestCase>>;

fn collect_test_case_from_subdirectory<F>(root: &str, subdir: &str, loader: F)
    -> TestCaseCollection
    where F: Fn(String) -> Box<TestCase>
{
    let fullpath = Path::new(root).join(subdir);
    read_dir(fullpath).unwrap()
        .map(|dir_entry| dir_entry.unwrap().path().as_path().to_owned())
        .filter(|path| path.extension().unwrap().to_str().unwrap() == "srv")
        .map(|path| loader(path.to_str().unwrap().to_owned()))
        .collect()
}

pub fn collect_test_cases(path: &str) -> TestCaseCollection {
    let mut v = Vec::new();
    v.extend(collect_test_case_from_subdirectory(path, "compile-pass", CompilePass::new));
    v.extend(collect_test_case_from_subdirectory(path, "compile-fail", CompileFail::new));
    v
}

