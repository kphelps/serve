use self::context::SemanticContext;
use self::scan::AstScanner;
use self::types::SemanticResult;
use self::type_check::TypeChecker;
use super::symbol::SymbolRegistry;
use super::ast::AST;

mod context;
mod environment;
mod scan;
mod type_check;
mod type_registrar;
mod types;

pub fn type_check(
    ast: AST,
    symbols: SymbolRegistry,
) -> SemanticResult {
    let mut ctx = SemanticContext::new(symbols);
    ctx.scan_declarations(&ast);
    ctx.type_check_declarations(&ast)
}
