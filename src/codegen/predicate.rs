//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use syn::{Ident, IntTy, Lit};
use quote;
use super::typed;
use std::collections::BTreeMap;

pub mod names {
    use ir;
    use syn::Ident;
    use codegen::camelize;

    // Fields of a predicate, using native names if present, or arg0-argN if not
    pub fn fields(pred: &ir::Predicate) -> Vec<Ident> {
        match pred.names {
            Some(ref names) => names.iter().cloned().map(Ident::new).collect(),
            None => pred.types
                .iter()
                .enumerate()
                .map(|x| Ident::new(format!("arg{}", x.0)))
                .collect(),
        }
    }

    // Name of a predicate's fact type
    pub fn fact(pred: &ir::Predicate) -> Ident {
        Ident::new(camelize(&pred.name))
    }

    // Name of a predicate's insertion function
    pub fn insert(pred: &ir::Predicate) -> Ident {
        Ident::new(format!("insert_{}", pred.name.to_lowercase()))
    }

    // Name of the field where the predicate's tuples are stored
    pub fn tuple(pred_name: &str) -> Ident {
        Ident::new(format!("pred_{}", pred_name.to_lowercase()))
    }

    // Name of the constant where the predicate's ID is stored
    pub fn id(pred_name: &str) -> Ident {
        Ident::new(format!("PRED_ID_{}", pred_name.to_uppercase()))
    }
}

// Generates a fact type and tuple conversion between it
pub fn fact(pred_id: usize, pred: &ir::Predicate) -> quote::Tokens {
    let fact_name = names::fact(pred);
    let fact_name2 = fact_name.clone();
    let pred_id_name = names::id(&pred.name);
    let pred_id_k = Lit::Int(pred_id as u64, IntTy::Usize);

    let field_types = pred.types
        .iter()
        .cloned()
        .map(Ident::new)
        .collect::<Vec<_>>();

    let field_names = names::fields(pred);
    let field_names2 = field_names.clone();

    let arity = Lit::Int(pred.types.len() as u64, IntTy::Usize);
    let arity2 = arity.clone();

    let mut type_stores = Vec::new();
    {
        for (index, (type_, field_name)) in pred.types.iter().zip(field_names.clone()).enumerate() {
            let store = typed::store(type_, &quote! {self.#field_name});
            let index_lit = Lit::Int(index as u64, IntTy::Usize);
            type_stores.push(quote! {
                out[#index_lit] = #store;
            });
        }
    }

    let type_loads = pred.types
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
        const #pred_id_name: usize = #pred_id_k;
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
                let mut out = [0; #arity2];
                #(#type_stores)*
                out
            }
        }
    }
}

// Generates an insertion function by using the tuple converter
pub fn insert(pred: &ir::Predicate) -> quote::Tokens {
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
