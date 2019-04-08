use syn::{Ident, IntTy, Lit};
use crate::codegen::snakize;
use quote;
// Whether the type can be cast into a usize rather than using typed storage
pub fn is_small(type_: &str) -> bool {
    match type_ {
        //TODO remove u64 from this list on 32-bit systems
        //TODO make list extensible by user programs
        "usize" | "u64" | "u32" | "u16" | "u8" | "i64" | "i32" | "i16" | "i8" | "bool" => true,
        _ => false,
    }
}

// Generates an expression to get the typed value from an indexed tuple, assuming the tuple is in
// the variable 'tuple', and the database in the variable 'db'
pub fn load(type_: &str, index: usize) -> quote::Tokens {
    let index_lit = Lit::Int(index as u64, IntTy::Usize);
    if type_ == "bool" {
        quote! {
            tuple[#index_lit] == 1
        }
    } else if is_small(type_) {
        let out_type = Ident::new(type_.to_string());
        quote! {
            tuple[#index_lit] as #out_type
        }
    } else {
        let data_name = name(type_);
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        quote! {
            &db.#data_name[tuple[#index_lit]]
        }
    }
}

// Generates an expression to turn a typed value into a key to construct a tuple with, assuming the
// database is in the variable 'db'
pub fn store(type_: &str, expr: &quote::Tokens) -> quote::Tokens {
    if is_small(type_) {
        quote! {
            #expr as usize
        }
    } else {
        let data_name = name(type_);
        quote! {
            db.#data_name.insert(#expr)
        }
    }
}

// Provides the name of typed storage for a given type
pub fn name(type_: &str) -> Ident {
    Ident::new(format!("data_{}", snakize(type_)))
}

// Gives a standard name to a field to hold the precomputed key for a constant
pub fn const_name(const_name: &str) -> Ident {
    Ident::new(format!("k_{}", snakize(const_name)))
}
