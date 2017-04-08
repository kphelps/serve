* Figure out the best way to get me to work on this long term.
JIRA? Github project? Trello? Something else?
Ideally, have a variety of things to work on
    - Small, 30 minute chunks
    - Some larger, weekend tasks

* Throw out codegen code (probably)
* Type checker
    * Expressions need to be implemented
    * Figure out how to tie the type checker into the runtime code?
    * No method calls for now
    * Scan declarations before type checking

Once type checking is somewhat functional:
* Define an intermediate representation
* Transpile AST -> IR
* Transpile IR -> Rust

* AST:
    * Assignment
    * Function definitions

MORE:

* Structs
* Method Calls
* Parametric Polymorphism (needed for containers)
* Type inference
* Rust <-> Serve bridge
    * Ideally used to expose standard library
    * Also usable at runtime
* Multi-file
    * MVP is concatenation
* Blocks!

:shipit:
