//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use quote;
use syn::{Ident, Lit, IntTy};
use std::collections::HashMap;

mod typed;
mod query;
mod predicate;
mod rule;

fn camelize(s: &str) -> String {
    let mut out_chars = Vec::new();
    let mut new_word = true;
    for c in s.chars() {
        match c {
            '_' => new_word = true,
            c if new_word => {
                new_word = false;
                out_chars.extend(c.to_uppercase());
            }
            c => out_chars.push(c),
        }
    }
    out_chars.iter().collect()
}

/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    use std::collections::HashSet;
    // Declarations of predicate fact structs
    let pred_fact_decls = prog.predicates
        .values()
        .map(predicate::fact)
        .collect::<Vec<_>>();
    // Inserter functions for fact structs
    let pred_inserts = prog.predicates
        .values()
        .map(predicate::insert)
        .collect::<Vec<_>>();
    // Declarations of query result structs
    let mut query_structs = Vec::new();
    // Full and partial query functions
    let mut query_funcs = Vec::new();
    // Database initializaiton to make sure indices are present
    let mut query_registrations = Vec::new();
    for gen in prog.queries.values().map(|query| query::gen(query)) {
        query_structs.push(gen.decls);
        query_funcs.push(gen.impls);
        query_registrations.push(gen.init)
    }

    let rule_funcs = prog.rules
        .values()
        .map(|rule| rule::gen(rule))
        .collect::<Vec<_>>();

    let pred_names = prog.predicates
        .values()
        .map(|pred| predicate::names::tuple(&pred.name))
        .collect::<Vec<_>>();
    let pred_names2 = pred_names.clone();

    // Collecting types to figure out what typed storage is needed
    let mut type_set = HashSet::new();
    for pred in prog.predicates.values() {
        for type_ in &pred.types {
            type_set.insert(type_.clone());
        }
    }

    // Name all typed storage which isn't small (those don't need it)
    let data_type_names = type_set
        .iter()
        .filter(|x| !typed::is_small(x))
        .map(|x| typed::name(x))
        .collect::<Vec<_>>();
    let data_type_names2 = data_type_names.clone();

    let type_names = type_set
        .into_iter()
        .filter(|type_| !typed::is_small(type_))
        .map(Ident::new)
        .collect::<Vec<_>>();

    // Make a place for queries to keep their runtime data (mailbox numbers, a copy of their
    // restrict so that they don't have to build it over and over)
    let query_storage_names = prog.queries
        .values()
        .map(query::names::store)
        .collect::<Vec<_>>();
    let query_storage_names2 = query_storage_names.clone();

    let arities = prog.predicates
        .values()
        .map(|pred| Lit::Int(pred.types.len() as u64, IntTy::Usize))
        .collect::<Vec<_>>();

    // Map from constant name to constant type
    let mut consts: HashMap<String, String> = HashMap::new();
    for (k, type_) in prog.queries.values().flat_map(|query| {
        query::consts(query, &prog.predicates)
    })
    {
        consts.insert(k, type_);
    }
    for (k, type_) in prog.rules.values().flat_map(|rule| {
        rule::consts(rule, &prog.predicates)
    })
    {
        consts.insert(k, type_);
    }

    let mut k_names: Vec<Ident> = Vec::new();
    let mut k_inits: Vec<quote::Tokens> = Vec::new();
    for (k, type_) in consts {
        let k_name = typed::const_name(&k);
        let k_ident = Ident::new(k);
        let k_expr = quote! { #k_ident };
        let k_store = typed::store(&type_, &k_expr);
        k_names.push(k_name.clone());
        k_inits.push(quote! {
            db.#k_name = #k_store;
        });
    }
    let k_names2 = k_names.clone();

    let rule_invokes: Vec<Ident> = prog.rules.values().map(rule::names::rule_invoke).collect();

    // TODO add naming feature for program so that mycroft can be invoked multiple times
    // in the same module.
    quote! {
       mod mycroft_program {
            #![allow(unused_imports,dead_code,unused_variables,unused_mut)]
            use mycroft_support::storage::{Tuples, Data};
            use mycroft_support::join::{Join, SkipIterator, Field, Restrict};
            #[derive(Default)]
            struct QueryStorage {
                mailboxes: Vec<usize>,
                restricts: Vec<Vec<Option<Restrict>>>,
            }
            pub struct Database {
                #(#pred_names: Tuples,)*
                #(#data_type_names: Data<#type_names>,)*
                #(#query_storage_names: QueryStorage,)*
                #(#k_names: usize,)*
            }
            #(#pred_fact_decls)*
            #(#query_structs)*
            impl Database {
                pub fn new() -> Self {
                    let mut db = Self {
                        #(#pred_names2: Tuples::new(#arities),)*
                        #(#data_type_names2: Data::new(),)*
                        #(#query_storage_names2: QueryStorage::default(),)*
                        #(#k_names2: ::std::usize::MAX,)*
                    };
                    // Constants must be initialized first, they are used in other initialization
                    #(#k_inits)*
                    #(#query_registrations)*
                    db
                }
                pub fn run_rules(&mut self) {
                    let mut productive = true;
                    while productive {
                        productive = false;
                        #(productive |= self.#rule_invokes();)*
                    }
                }
                #(#pred_inserts)*
                #(#query_funcs)*
                #(#rule_funcs)*
            }
        }
    }
}
