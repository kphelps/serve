#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]

extern crate aster;

extern crate syntex_syntax as syntax;

fn main() {
    let builder = aster::AstBuilder::new();
    let stmt = builder.item().extern_crate("function")
        .build()
    ;

    println!("{}", syntax::print::pprust::item_to_string(&stmt));
}
