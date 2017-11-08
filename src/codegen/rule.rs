use ir;
use quote;
//use syn::{Ident, Lit, IntTy};
use std::collections::HashMap;

pub fn consts(rule: &ir::Rule, preds: &HashMap<String, ir::Predicate>) -> Vec<(String, String)> {
    // Our query was registered seperately, so we don't need to to examine it for consts
    let pred = &preds[&rule.head_pred];
    let mut out = Vec::new();
    for (idx, hv) in rule.head_vals.iter().enumerate() {
        match *hv {
            ir::HeadVal::Const(ref k) => {
                out.push((k.to_string(), pred.types[idx].to_string()));
            }
            _ => (),
        }
    }
    out
}

pub fn gen(_rule: &ir::Rule) -> quote::Tokens {
    unimplemented!()
    /*
    quote! {
        fn #rule_substitute_name(&self, tuple: Vec<usize>) -> Vec<usize> {
            vec![#(#tuple_subs),*]
        }
        pub fn #rule_invoke_name(&self) -> bool {
            let mut productive = false;
            let tuples = self.#query_incr_tuple_name();
            for tuple in tuples {
                productive |= self.#tuple_name.insert(self.#rule_substitute_name2(tuple)).1;
            }
            productive
        }
    }
    */
}
