//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use quote;
use syn::{Ident, IntTy, Lit, StrStyle};
use std::collections::BTreeMap;

mod typed;
mod query;
mod predicate;
mod rule;

fn camelize(s: &str) -> String {
    let mut out_chars = Vec::new();
    let mut new_word = true;
    for c in s.chars() {
        match c {
            '_' | ':' => new_word = true,
            c if new_word => {
                new_word = false;
                out_chars.extend(c.to_uppercase());
            }
            c => out_chars.push(c),
        }
    }
    out_chars.into_iter().collect()
}

fn snakize(s: &str) -> String {
    let mut case = true;
    let mut out_chars = Vec::new();
    for c in s.chars() {
        match c {
            c if !c.is_alphanumeric() => case = false,
            c if c.is_uppercase() && !case => {
                out_chars.push('_');
                out_chars.extend(c.to_lowercase());
                case = true;
            }
            c => {
                case = c.is_uppercase();
                out_chars.extend(c.to_lowercase());
            }
        }
    }
    out_chars.into_iter().collect()
}

/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    use std::collections::BTreeSet;
    // Declarations of predicate fact structs
    let pred_fact_decls = prog.predicates
        .values()
        .enumerate()
        .map(|(i, f)| predicate::fact(i, f))
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
        .enumerate()
        .map(|(rule_id, rule)| rule::gen(rule_id, rule))
        .collect::<Vec<_>>();

    let pred_names = prog.predicates
        .values()
        .map(|pred| predicate::names::tuple(&pred.name))
        .collect::<Vec<_>>();
    let pred_names2 = pred_names.clone();
    let pred_names3 = pred_names.clone();

    let pred_ids = pred_names
        .iter()
        .enumerate()
        .map(|(pid, _)| Lit::Int(pid as u64, IntTy::Usize))
        .collect::<Vec<_>>();
    let pred_ids2 = pred_ids.clone();

    let mut pred_name_to_id: BTreeMap<&str, usize> = BTreeMap::new();
    for (pid, name) in prog.predicates.keys().enumerate() {
        pred_name_to_id.insert(name, pid);
    }

    let rule_preds = prog.rules
        .values()
        .enumerate()
        .map(|(rule_id, rule)| {
            rule::preds(
                rule_id,
                &prog.queries[rule.body_query.as_str()],
                &pred_name_to_id,
            )
        })
        .collect::<Vec<_>>();

    // Collecting types to figure out what typed storage is needed
    let mut type_set = BTreeSet::new();
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
    let data_type_names3 = data_type_names.clone();
    let data_type_names4 = data_type_names.clone();

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
        .map(|pred| {
            let mut build_aggs = Vec::new();
            for (idx, agg) in pred.aggs.iter().enumerate() {
                match agg {
                    &Some(ref agg_func) => {
                        let agg_name = Ident::new(agg_func.to_string());
                        let type_ = &pred.types[idx];
                        if typed::is_small(type_) {
                            build_aggs.push(quote! {
                                aggs.push(Some(Box::new(::mycroft_support::aggregator::Func::new(#agg_name))))
                            })
                        } else {
                            let data = typed::name(type_);
                            build_aggs.push(quote! {
                                aggs.push(Some(Box::new(::mycroft_support::aggregator::FuncData::new(#agg_name, #data.clone()))))
                            })
                        }
                    }
                    &None => build_aggs.push(quote! { aggs.push(None) }),
                }
            }
            quote! {
                {
                    use mycroft_support::aggregator::Aggregator;
                    let mut aggs: Vec<Option<Box<Aggregator>>> = Vec::new();
                    #(#build_aggs;)*
                    aggs
                }
            }
        })
        .collect::<Vec<_>>();

    // Map from constant name to constant type
    let mut consts: BTreeMap<String, String> = BTreeMap::new();
    for (k, type_) in prog.queries
        .values()
        .flat_map(|query| query::consts(query, &prog.predicates))
    {
        consts.insert(k, type_);
    }
    for (k, type_) in prog.rules
        .values()
        .flat_map(|rule| rule::consts(rule, &prog.predicates))
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

    let rule_decls = prog.rules
        .values()
        .map(rule::result_type)
        .collect::<Vec<_>>();

    let fact_names: Vec<Ident> = prog.predicates
        .values()
        .map(predicate::names::fact)
        .collect();
    let fact_names2 = fact_names.clone();
    let fact_names3 = fact_names.clone();
    let fact_names4 = fact_names.clone();

    let mut pred_name_strs = Vec::new();
    let mut pred_id_ks = Vec::new();
    for (i, s) in prog.predicates.keys().enumerate() {
        pred_name_strs.push(Lit::Str(s.clone(), StrStyle::Cooked));
        pred_id_ks.push(Lit::Int(i as u64, IntTy::Usize));
    }

    let mut rule_name_strs = Vec::new();
    let mut rule_id_ks = Vec::new();
    for (i, s) in prog.rules.keys().enumerate() {
        rule_name_strs.push(Lit::Str(s.clone(), StrStyle::Cooked));
        rule_id_ks.push(Lit::Int(i as u64, IntTy::Usize));
    }

    // TODO add naming feature for program so that mycroft can be invoked multiple times
    // in the same module.
    quote! {
       mod mycroft_program {
            #![allow(unused_imports,dead_code,unused_variables,unused_mut,unused_unsafe)]
            use mycroft_support::storage::{Tuples, Data, Provenance};
            use mycroft_support::join::{Join, SkipIterator, Field, Restrict};
            use mycroft_support::derivation::{Derivation, RawDerivation, Fact};
            use std::collections::HashSet;
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
            #(#rule_decls)*
            #[derive(Debug)]
            pub enum AnyFact {
                #(#fact_names(#fact_names2),)*
            }
            fn rule_slot_to_pred(rule_id: usize, slot: usize) -> usize {
                match (rule_id, slot) {
                    #(#rule_preds)*
                    _ => panic!("Internal error, unmatched rule_id/slot combo")
                }
            }
            fn pred_name_to_id(pred_name: &str) -> usize {
                match pred_name {
                    #(#pred_name_strs => #pred_id_ks,)*
                    _ => panic!("Unmatched pred name")
                }
            }
            fn rule_id_to_name(rule_id: usize) -> &'static str {
                match rule_id {
                    #(#rule_id_ks => #rule_name_strs,)*
                    _ => panic!("Internal error, unmatched pred id")
                }
            }
            impl Database {
                pub fn new() -> Self {
                    #(let #data_type_names2 = Data::new();)*
                    let mut db = Self {
                        #(#pred_names2: Tuples::new(#arities),)*
                        #(#data_type_names3: #data_type_names4,)*
                        #(#query_storage_names2: QueryStorage::default(),)*
                        #(#k_names2: ::std::usize::MAX,)*
                    };
                    // Constants must be initialized first, they are used in other initialization
                    #(#k_inits)*
                    #(#query_registrations)*
                    db
                }
                fn tuple_by_id(&self, key: usize) -> &Tuples {
                    match key {
                        #(#pred_ids => &self.#pred_names3,)*
                        _ => panic!("Internal error, unbound predicate ID")
                    }
                }
                fn raw_derivation(&self, fact: &Fact) -> RawDerivation {
                    RawDerivation::from_storage(fact, &|key| self.tuple_by_id(key),
                       &rule_slot_to_pred, HashSet::new(), 2).expect("No valid derivation for fact?")
                }
                fn project_fact(&self, f: &Fact) -> AnyFact {
                    let tuple = self.tuple_by_id(f.predicate_id).get(f.fact_id);
                    match f.predicate_id {
                        #(#pred_ids2 => AnyFact::#fact_names3(
                                #fact_names4::from_tuple(self, &tuple)),)*
                        _ => panic!("Internal error, unbound predicate ID in fact projection")
                    }
                }
                pub fn derivation(&self, fact: &Fact) -> Derivation<AnyFact> {
                    let raw = self.raw_derivation(fact);
                    Derivation::from_raw(raw, &|f| self.project_fact(f), &rule_id_to_name)
                }
                pub fn run_rules_once(&mut self) -> Vec<Fact> {
                    let mut productive = Vec::new();
                    #(productive.extend(&self.#rule_invokes());)*
                    productive
                }
                pub fn run_rules(&mut self) {
                    while !self.run_rules_once().is_empty() {}
                }
                #(#pred_inserts)*
                #(#query_funcs)*
                #(#rule_funcs)*
            }
        }
    }
}
