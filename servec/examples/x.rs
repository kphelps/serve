#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]

extern crate aster;

extern crate syntex_syntax as syntax;

fn main() {
    let builder = aster::AstBuilder::new();
    let stmt = builder.item().fn_("function")
        .arg_id("x").ty().isize()
        .arg_id("y").ty().isize()
        .return_().isize()
        .block().with_stmts(vec![])
        .build()
    ;

    println!("{}", syntax::print::pprust::item_to_string(&stmt));
}
