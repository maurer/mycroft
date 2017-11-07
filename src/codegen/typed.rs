use syn::{Ident, Lit, IntTy};
use quote;
pub fn is_small(type_: &str) -> bool {
    match type_ {
        //TODO remove u64 from this list on 32-bit systems
        //TODO make list extensible by user programs
        "usize" | "u64" | "u32" | "u16" | "u8" | "i64" | "i32" | "i16" | "i8" => true,
        _ => false,
    }
}

pub fn load(type_: &str, index: usize) -> quote::Tokens {
    let index_lit = Lit::Int(index as u64, IntTy::Usize);
    if is_small(type_) {
        let out_type = Ident::new(type_.clone());
        quote! {
            tuple[#index_lit] as #out_type
        }
    } else {
        let data_name = name(type_);
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        quote! {
            db.#data_name.get(tuple[#index_lit]).clone()
        }
    }
}

pub fn store(type_: &str, expr: quote::Tokens) -> quote::Tokens {
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

pub fn name(type_: &str) -> Ident {
    Ident::new(format!("data_{}", type_.to_lowercase()))
}
