extern crate colored;
extern crate libserve;

mod collector;
mod compile_pass;
mod compile_fail;
mod test_case;

use colored::*;
use self::collector::collect_test_cases;

fn main() {
    let test_cases = collect_test_cases("tests");
    let mut failures = Vec::new();
    println!("{}", "Running `serve` compiler tests...".blue().bold());
    println!("");
    let test_case_count = test_cases.len();
    for test_case in test_cases {
        let result = test_case.execute();
        if result.is_err() {
            failures.push(test_case.name().to_string());
            println!("{}", format!("Error in `{}`:", test_case.name()).red().bold());
            println!("{}", format!("{:?}", result));
            println!("");
        }
    }
    if failures.is_empty() {
        println!("{}", format!("{} tests passed!", test_case_count).green().bold());
    } else {
        println!("{}", format!("{} tests failed:", failures.len()).red().bold());
        for failure in failures {
            println!("\t{}", failure.red());
        }
    }
    println!("");
}
