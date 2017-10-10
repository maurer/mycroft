//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use quote;
use syn::Ident;

fn predicate(pred: &ir::Predicate) -> quote::Tokens {
    let name = Ident::new(pred.name.clone());
    let vec_types = pred.types_
        .iter()
        .map(|type_| {
            let type_q = Ident::new(type_.clone());
            quote! {Vec<#type_q>}
        })
        .collect::<Vec<_>>();
    let types = pred.types_
        .iter()
        .cloned()
        .map(Ident::new)
        .collect::<Vec<_>>();
    let names: Vec<_> = match pred.names {
        Some(ref names) => names.iter().cloned().map(Ident::new).collect(),
        None => {
            pred.types_
                .iter()
                .enumerate()
                .map(|x| Ident::new(format!("arg{}", x.0)))
                .collect()
        }
    };
    // TODO: is there a way to avoid cloning here and refer to the same names field?
    let field_names = names.clone();
    let field_names_2a = names.clone();
    let field_names_2b = names.clone();
    let field_names_3a = names.clone();
    let field_names_3b = names.clone();
    let new_names = names.clone();
    quote! {
        pub mod #name {
            pub struct Storage {
                len: usize,
                #(#names: #vec_types),*
            }
            pub struct Fact {
                #(pub #field_names: #types),*
            }
            impl Storage {
                pub fn new() -> Self {
                    Storage {
                        len: 0,
                        #(#new_names: Vec::new()),*
                    }
                }
                pub fn insert(&mut self, fact: Fact) -> (usize, bool) {
                    // Check for presence
                    for i in 0..self.len {
                        if #(self.#field_names_2a[i] == fact.#field_names_2b)&&* {
                            return (i, false)
                        }
                    }
                    // It's not here, insert it.
                    #(self.#field_names_3a.push(fact.#field_names_3b));* ;
                    self.len += 1;
                    (self.len - 1, true)
                }
            }
        }
    }
}

/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    let pred_decls = prog.predicates.values().map(predicate).collect::<Vec<_>>();
    quote! {
        mod predicates {
            #(#pred_decls)*
        }
    }
}
