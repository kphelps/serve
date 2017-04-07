use super::super::ast::*;
use super::context::SemanticContext;
use super::type_registrar::TypeRegistrar;
use super::types::{ValueEntry};

pub trait AstScanner {
    fn scan_declarations(&mut self, decls: &Vec<Declaration>);
    fn scan_declaration(&mut self, decls: &Declaration);
    fn scan_application_statements(&mut self, decls: &Vec<ApplicationStatement>);
    fn scan_application_statement(&mut self, decls: &ApplicationStatement);
}

impl AstScanner for SemanticContext {

    fn scan_declarations(&mut self, decls: &Vec<Declaration>) {
        for decl in decls {
            self.scan_declaration(decl);
        }
    }

    fn scan_declaration(&mut self, decl: &Declaration) {
        match *decl {
            Declaration::Application(ref name, ref body) => {
                self.scan_application_statements(body)
            },
            _ => (),
        }
    }

    fn scan_application_statements(&mut self, stmts: &Vec<ApplicationStatement>) {
        for stmt in stmts {
            self.scan_application_statement(stmt);
        }
    }

    fn scan_application_statement(&mut self, stmt: &ApplicationStatement) {
        match *stmt {
            ApplicationStatement::Endpoint(ref name, ref args, ref return_type_name, ref body) => {
                let arg_types = self.extract_parameter_types(args);
                let return_type = self.get_type(*return_type_name);
                self.environment.insert_value(
                    *name,
                    ValueEntry::Endpoint(arg_types, return_type)
                );
            },
            _ => (),
        }
    }

}
