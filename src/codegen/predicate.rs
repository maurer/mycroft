//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use super::typed;
use crate::codegen::ident_new;
use crate::ir;
use quote;
use std::collections::BTreeMap;

pub mod names {
    use crate::codegen::{camelize, ident_new};
    use crate::ir;
    use syn::Ident;

    // Fields of a predicate, using native names if present, or arg0-argN if not
    pub fn fields(pred: &ir::Predicate) -> Vec<Ident> {
        match pred.names {
            Some(ref names) => names.iter().map(|n| ident_new(n)).collect(),
            None => pred
                .types
                .iter()
                .enumerate()
                .map(|x| ident_new(&format!("arg{}", x.0)))
                .collect(),
        }
    }

    // Name of a predicate's fact type
    pub fn fact(pred: &ir::Predicate) -> Ident {
        ident_new(&camelize(&pred.name))
    }

    // Name of a predicate's insertion function
    pub fn insert(pred: &ir::Predicate) -> Ident {
        ident_new(&format!("insert_{}", pred.name.to_lowercase()))
    }

    // Name of the field where the predicate's tuples are stored
    pub fn tuple(pred_name: &str) -> Ident {
        ident_new(&format!("pred_{}", pred_name.to_lowercase()))
    }

    // Name of the constant where the predicate's ID is stored
    pub fn id(pred_name: &str) -> Ident {
        ident_new(&format!("PRED_ID_{}", pred_name.to_uppercase()))
    }
}

// Generates a fact type and tuple conversion between it
pub fn fact(pred_id: usize, pred: &ir::Predicate) -> proc_macro2::TokenStream {
    let fact_name = names::fact(pred);
    let fact_name2 = fact_name.clone();
    let pred_id_name = names::id(&pred.name);

    let field_types = pred.types.iter().map(|n| ident_new(n)).collect::<Vec<_>>();

    let field_names = names::fields(pred);
    let field_names2 = field_names.clone();

    let arity = pred.types.len();

    let mut type_stores = Vec::new();
    {
        for (index, (type_, field_name)) in pred.types.iter().zip(field_names.clone()).enumerate() {
            let store = typed::store(type_, &quote! {self.#field_name});
            type_stores.push(quote! {
                out[#index] = #store;
            });
        }
    }

    let type_loads = pred
        .types
        .iter()
        .enumerate()
        .map(|(idx, type_)| typed::load(type_, idx))
        .collect::<Vec<_>>();
    //TODO dedup release generation
    let mut type_counts = BTreeMap::new();
    for type_ in pred.types.iter() {
        if !typed::is_small(type_) {
            *type_counts.entry(type_.to_string()).or_insert(0usize) += 1;
        }
    }
    let mut type_releases = Vec::new();
    for (type_, count) in type_counts {
        let data_name = typed::name(&type_);
        type_releases.push(quote! {
            db.#data_name.read_exit(#count);
        });
    }
    quote! {
        const #pred_id_name: usize = #pred_id;
        #[derive(Debug)]
        pub struct #fact_name {
            #(pub #field_names: #field_types),*
        }
        impl #fact_name2 {
            fn from_tuple(db: &Database, tuple: &[usize]) -> Self {
                let out = Self {
                    #(#field_names2: (#type_loads).clone()),*
                };
                unsafe {
                    #(#type_releases;)*
                }
                out
            }
            fn to_tuple(self, db: &mut Database) -> [usize; #arity] {
                let mut out = [0; #arity];
                #(#type_stores)*
                out
            }
        }
    }
}

// Generates an insertion function by using the tuple converter
pub fn insert(pred: &ir::Predicate) -> proc_macro2::TokenStream {
    let insert_name = names::insert(pred);
    let fact_name = names::fact(pred);
    let tuple_name = names::tuple(&pred.name);
    let pid = names::id(&pred.name);
    quote! {
        pub fn #insert_name(&mut self, fact: #fact_name) -> usize {
            let tuple = fact.to_tuple(self);
            let res = self.#tuple_name.insert(&tuple, Provenance::Base);
            self.purge_mid(#pid, res.2, res.0);
            res.0
        }
    }
}
