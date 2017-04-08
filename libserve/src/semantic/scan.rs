use super::super::ast::*;
use super::super::symbol::Symbol;
use super::context::SemanticContext;
use super::type_registrar::TypeRegistrar;
use super::types::{ServeType, ValueEntry};

pub trait AstScanner {
    fn scan_top_level_declarations(&mut self, decls: &Vec<TopLevelDeclaration>);
}

impl AstScanner for SemanticContext {

    fn scan_top_level_declarations(&mut self, decls: &Vec<TopLevelDeclaration>) {
        for decl in decls {
            self.scan_top_level_declaration(decl);
        }
    }

}

impl SemanticContext {

    fn scan_top_level_declaration(&mut self, decl: &TopLevelDeclaration) {
        match *decl {
            TopLevelDeclaration::Application(ref name, ref body) => {
                self.scan_application_statements(body)
            },
            TopLevelDeclaration::Declaration(ref inner) => {
                self.scan_declaration(inner)
            }
            _ => (),
        }
    }

    fn scan_declaration(&mut self, stmt: &Declaration) {
        match *stmt {
            Declaration::Function(ref name, ref args, ref return_type_name, _) => {
                self.register_function_decl(name, args, return_type_name, ValueEntry::Function);
            }
        }
    }

    fn scan_application_statements(&mut self, stmts: &Vec<ApplicationStatement>) {
        for stmt in stmts {
            self.scan_application_statement(stmt);
        }
    }

    fn scan_application_statement(&mut self, stmt: &ApplicationStatement) {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref return_type_name, _) => {
                self.register_function_decl(name, args, return_type_name, ValueEntry::Endpoint);
            },
            ApplicationStatement::Declaration(ref inner) => {
                self.scan_declaration(inner)
            }
            _ => (),
        }
    }

    fn register_function_decl<F>(
        &mut self,
        name: &Symbol,
        args: &Vec<FunctionParameter>,
        return_type_name: &Symbol,
        ctor: F,
    ) where F: Fn(Vec<ServeType>, ServeType) -> ValueEntry
    {
        let arg_types = self.extract_parameter_types(args);
        let return_type = self.get_type(*return_type_name);
        self.environment.insert_value(
            *name,
            ctor(arg_types, return_type)
        );
    }

}
