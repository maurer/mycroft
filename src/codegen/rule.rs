use ir;
use quote;
use syn::{Lit, IntTy, Ident};
use std::collections::HashMap;
use super::{predicate, query, typed};

pub mod names {
    use ir;
    use syn::Ident;
    use codegen::camelize;

    pub fn rule_invoke(rule: &ir::Rule) -> Ident {
        Ident::new(format!("rule_invoke_{}", rule.name))
    }

    pub fn func_result(rule: &ir::Rule) -> Ident {
        Ident::new(format!(
            "{}Out",
            camelize(rule.func.as_ref().unwrap().as_str())
        ))
    }

    pub fn func_in(rule: &ir::Rule) -> Ident {
        Ident::new(format!(
            "{}In",
            camelize(rule.func.as_ref().unwrap().as_str())
        ))
    }
}

pub fn consts(rule: &ir::Rule, preds: &HashMap<String, ir::Predicate>) -> Vec<(String, String)> {
    // Our query was registered seperately, so we don't need to to examine it for consts
    let pred = &preds[&rule.head_pred];
    let mut out = Vec::new();
    for (idx, hv) in rule.head_vals.iter().enumerate() {
        if let ir::MatchVal::Const(ref k) = *hv {
            out.push((k.to_string(), pred.types[idx].to_string()));
        }
    }
    out
}

pub fn result_type(rule: &ir::Rule) -> quote::Tokens {
    // TODO: dedup between this function and Predicate and Query owned results
    if rule.func.is_none() {
        return quote!{};
    }
    let out_name = names::func_result(rule);
    let out_name2 = out_name.clone();

    let in_name = names::func_in(rule);

    let query_view_name = query::names::result_borrow(&rule.body_query);

    let func_vars = rule.func_vars
        .iter()
        .map(|x| Ident::new(x.clone()))
        .collect::<Vec<_>>();
    let func_types = rule.func_types
        .iter()
        .map(|x| Ident::new(x.clone()))
        .collect::<Vec<_>>();

    let mut stores = Vec::new();

    for (index, (type_, field_name)) in rule.func_types.iter().zip(func_vars.clone()).enumerate() {
        let store = typed::store(type_, &quote! {self.#field_name});
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        stores.push(quote! {
                out[#index_lit] = #store;
            });
    }

    let arity = Lit::Int(func_vars.len() as u64, IntTy::Usize);
    let arity2 = arity.clone();

    quote! {
        pub struct #out_name {
            #(pub #func_vars: #func_types,)*
        }
        impl #out_name2 {
            fn to_tuple(self, db: &mut Database) -> [usize; #arity] {
                let mut out = [0; #arity2];
                #(#stores)*
                out
            }
        }

        pub type #in_name<'a> = #query_view_name<'a>;
    }
}

pub fn gen(rule: &ir::Rule) -> quote::Tokens {
    let tuple_subs: Vec<quote::Tokens> = rule.head_vals
        .iter()
        .map(|hv| match *hv {
            ir::MatchVal::Const(ref k) => {
                let k = typed::const_name(k);
                quote! { self.#k }
            }
            ir::MatchVal::Var(v) => {
                let v = Lit::Int(v as u64, IntTy::Usize);
                quote! { tuple[#v] }
            }
        })
        .collect();
    let rule_invoke_name = names::rule_invoke(rule);
    let tuple_name = predicate::names::tuple(&rule.head_pred);
    let query_incr_tuple_name = query::names::incr_tuple(&rule.body_query);

    let tuple_action = match rule.func {
        Some(ref name) => {
            let func = Ident::new(name.clone());
            let view = query::names::result_borrow(&rule.body_query);
            quote! {
                for extra_vars in #func(#view::from_tuple(self, tuple.clone())) {
                    let mut tuple = tuple.clone();
                    tuple.extend(&extra_vars.to_tuple(self));
                    productive |= self.#tuple_name.insert(&[#(#tuple_subs),*]).1
                }
            }
        }
        None => {
            quote! {
            productive |= self.#tuple_name.insert(&[#(#tuple_subs),*]).1;
        }
        }
    };

    quote! {
        pub fn #rule_invoke_name(&mut self) -> bool {
            let mut productive = false;
            let tuples = self.#query_incr_tuple_name();
            for tuple in tuples {
                #tuple_action
            }
            productive
        }
    }
}
