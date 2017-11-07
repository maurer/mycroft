//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use syn::{Ident, Lit, IntTy};
use quote;
use super::typed;
fn fields(pred: &ir::Predicate) -> Vec<Ident> {
    match pred.names {
        Some(ref names) => names.iter().cloned().map(Ident::new).collect(),
        None => {
            pred.types
                .iter()
                .enumerate()
                .map(|x| Ident::new(format!("arg{}", x.0)))
                .collect()
        }
    }
}

fn fact_name(pred: &ir::Predicate) -> Ident {
    Ident::new(pred.name.clone())
}

fn insert_name(pred: &ir::Predicate) -> Ident {
    Ident::new(format!("insert_{}", pred.name.to_lowercase()))
}

pub fn tuple_name(pred: &ir::Predicate) -> Ident {
    Ident::new(format!("pred_{}", pred.name.to_lowercase()))
}

pub fn fact(pred: &ir::Predicate) -> quote::Tokens {
    let fact_name = fact_name(pred);
    let fact_name2 = fact_name.clone();

    let field_types = pred.types
        .iter()
        .cloned()
        .map(Ident::new)
        .collect::<Vec<_>>();

    let field_names = fields(pred);
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

    quote! {
        pub struct #fact_name {
            #(pub #field_names: #field_types),*
        }
        impl #fact_name2 {
            fn from_tuple(db: &Database, tuple: &[usize]) -> Self {
                Self {
                    #(#field_names2: #type_loads),*
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

pub fn insert(pred: &ir::Predicate) -> quote::Tokens {
    let insert_name = insert_name(pred);
    let fact_name = fact_name(pred);
    let tuple_name = tuple_name(pred);
    quote! {
        pub fn #insert_name(&mut self, fact: #fact_name) -> usize {
            let tuple = fact.to_tuple(self);
            self.#tuple_name.insert(&tuple).0
        }
    }
}
