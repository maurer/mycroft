use ir;
use quote;
use syn::{Lit, IntTy};
use std::collections::HashMap;
use super::{predicate, query, typed};

pub mod names {
    use ir;
    use syn::Ident;
    pub fn rule_invoke(rule: &ir::Rule) -> Ident {
        Ident::new(format!("rule_invoke_{}", rule.name))
    }
}

pub fn consts(rule: &ir::Rule, preds: &HashMap<String, ir::Predicate>) -> Vec<(String, String)> {
    // Our query was registered seperately, so we don't need to to examine it for consts
    let pred = &preds[&rule.head_pred];
    let mut out = Vec::new();
    for (idx, hv) in rule.head_vals.iter().enumerate() {
        if let ir::HeadVal::Const(ref k) = *hv {
            out.push((k.to_string(), pred.types[idx].to_string()));
        }
    }
    out
}

pub fn gen(rule: &ir::Rule) -> quote::Tokens {
    let tuple_subs: Vec<quote::Tokens> = rule.head_vals
        .iter()
        .map(|hv| match *hv {
            ir::HeadVal::Const(ref k) => {
                let k = typed::const_name(k);
                quote! { self.#k }
            }
            ir::HeadVal::Var(v) => {
                let v = Lit::Int(v as u64, IntTy::Usize);
                quote! { tuple[#v] }
            }
        })
        .collect();
    let rule_invoke_name = names::rule_invoke(rule);
    let tuple_name = predicate::names::tuple(&rule.head_pred);
    let query_incr_tuple_name = query::names::incr_tuple(&rule.body_query);
    quote! {
        pub fn #rule_invoke_name(&mut self) -> bool {
            let mut productive = false;
            let tuples = self.#query_incr_tuple_name();
            for tuple in tuples {
                productive |= self.#tuple_name.insert(&[#(#tuple_subs),*]).1;
            }
            productive
        }
    }
}
