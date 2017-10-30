//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use quote;
use syn::{Ident, Lit, IntTy};

fn pred_fields(pred: &ir::Predicate) -> Vec<Ident> {
    match pred.names {
        Some(ref names) => names.iter().cloned().map(Ident::new).collect(),
        None => {
            pred.types_
                .iter()
                .enumerate()
                .map(|x| Ident::new(format!("arg{}", x.0)))
                .collect()
        }
    }
}

fn predicate_fact(pred: &ir::Predicate) -> quote::Tokens {
    let name = Ident::new(pred.name.clone());
    let types = pred.types_
        .iter()
        .cloned()
        .map(Ident::new)
        .collect::<Vec<_>>();
    let names = pred_fields(pred);
    let name2 = name.clone();
    let arity = Lit::Int(pred.types_.len() as u64, IntTy::Usize);
    let mut type_stores = Vec::new();
    for (index, (type_, field_name)) in pred.types_.iter().zip(names.clone()).enumerate() {
        let data_name = Ident::new(format!("data_{}", type_.to_lowercase()));
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        type_stores.push(quote! {
            out[#index_lit] = db.#data_name.insert(self.#field_name);
        })
    }
    let mut type_loads = Vec::new();
    for (index, type_) in pred.types_.iter().enumerate() {
        let data_name = Ident::new(format!("data_{}", type_.to_lowercase()));
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        type_loads.push(quote! {
            db.#data_name.get(tuple[#index_lit]).clone()
        })
    }

    let names2 = names.clone();
    let arity2 = arity.clone();
    quote! {
        pub struct #name {
            #(pub #names: #types),*
        }
        impl #name2 {
            fn from_tuple(db: &Database, tuple: &[usize]) -> Self {
                Self {
                    #(#names2: #type_loads),*
                }
            }
            fn to_tuple(self, db: &mut Database) -> [usize; #arity] {
                let mut out = [0; #arity2];
                #(#type_stores)*
                out
            }
        }
    }
}

fn predicate_insert(pred: &ir::Predicate) -> quote::Tokens {
    let insert_name = Ident::new(format!("insert_{}", pred.name.to_lowercase()));
    let fact_name = Ident::new(pred.name.clone());
    let pred_name = Ident::new(format!("pred_{}", pred.name.to_lowercase()));
    quote! {
        pub fn #insert_name(&mut self, fact: #fact_name) -> usize {
            let tuple = fact.to_tuple(self);
            self.#pred_name.insert(&tuple).0
        }
    }
}

/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    use std::collections::HashSet;
    let pred_fact_decls = prog.predicates
        .values()
        .map(predicate_fact)
        .collect::<Vec<_>>();
    let pred_inserts = prog.predicates
        .values()
        .map(predicate_insert)
        .collect::<Vec<_>>();
    let pred_names = prog.predicates
        .keys()
        .map(|name| Ident::new(format!("pred_{}", name.to_lowercase())))
        .collect::<Vec<_>>();
    let pred_names2 = pred_names.clone();

    let mut type_set = HashSet::new();
    for pred in prog.predicates.values() {
        for type_ in pred.types_.iter() {
            type_set.insert(type_.clone());
        }
    }
    let data_type_names = type_set
        .iter()
        .map(|type_name| {
            Ident::new(format!("data_{}", type_name.to_lowercase()))
        })
        .collect::<Vec<_>>();
    let data_type_names2 = data_type_names.clone();
    let type_names = type_set
        .into_iter()
        .map(|type_name| Ident::new(type_name))
        .collect::<Vec<_>>();

    let arities = prog.predicates
        .values()
        .map(|pred| Lit::Int(pred.types_.len() as u64, IntTy::Usize))
        .collect::<Vec<_>>();

    // TODO add naming feature for program so that mycroft can be invoked multiple times
    // in the same module.
    quote! {
       mod mycroft_program {
            use mycroft_support::storage::{Tuples, Data};
            pub struct Database {
                #(#pred_names: Tuples),*,
                #(#data_type_names: Data<#type_names>),*
            }
            #(#pred_fact_decls)*
            impl Database {
                pub fn new() -> Self {
                    Self {
                        #(#pred_names2: Tuples::new(#arities)),*,
                        #(#data_type_names2: Data::new()),*
                    }
                }
                #(#pred_inserts)*
            }
        }
    }
}
