//! The IR module describes the internal representation of the program.
//! Transformation into the IR will also check the AST for validity, so errors may be returned in
//! some cases.

use std::collections::HashMap;

use ast;

/// `error-chain`-generated error module for the checks occurring during transformation from
/// AST into IR.
pub mod error {
    use ast;
    error_chain! {
        errors {
            #[doc = "This error is generated if a predicate is defined twice"]
            PredicateDefinedTwice(first: ast::Predicate, second: ast::Predicate) {
                description("Predicate defined twice."),
                display("{} already defined as {}", second, first),
            }
        }
    }
}
use self::error::*;

/// IR Predicate Representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// AST of the predicate, for use in error reporting
    pub ast: ast::Predicate,
    /// Field names, if they exist. If this field is `Some`, the vector length will be equal to
    /// that of `types_`
    pub names: Option<Vec<String>>,
    /// Types of the predicate fields
    pub types_: Vec<String>,
}

impl Predicate {
    /// Transforms predicate AST into predicate IR
    pub fn from_ast(ast_pred: ast::Predicate) -> Self {
        let name = ast_pred.name.clone();
        let (names, types_) = match ast_pred.fields {
            ast::Fields::Ordered(ref types_) => (None, types_.clone()),
            ast::Fields::Named(ref fields) => {
                let mut names = Vec::new();
                let mut types_ = Vec::new();
                for field in fields.iter() {
                    names.push(field.name.clone());
                    types_.push(field.type_.clone());
                }
                (Some(names), types_)
            }
        };
        Predicate {
            name: name,
            ast: ast_pred,
            names: names,
            types_: types_,
        }
    }
}

/// IR Program representation
/// This is the structure to be handed off to the code generator.
pub struct Program {
    /// Map from predicate name to IR predicate
    pub predicates: HashMap<String, Predicate>,
}

impl Program {
    /// Generate a program IR from an AST
    pub fn from_ast(ast: ast::Program) -> Result<Self> {
        let mut predicates: HashMap<String, Predicate> = HashMap::new();
        for ast_pred in ast.predicates {
            let ir_pred = Predicate::from_ast(ast_pred);
            if predicates.contains_key(&ir_pred.name) {
                let first = predicates.remove(&ir_pred.name).unwrap().ast;
                let second = ir_pred.ast;
                return Err(ErrorKind::PredicateDefinedTwice(first, second).into());
            }
            predicates.insert(ir_pred.name.clone(), ir_pred);
        }
        Ok(Program { predicates: predicates })
    }
}
