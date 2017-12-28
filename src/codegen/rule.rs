use ir;
use quote;
use syn::{Ident, IntTy, Lit};
use std::collections::BTreeMap;
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

pub fn consts(rule: &ir::Rule, preds: &BTreeMap<String, ir::Predicate>) -> Vec<(String, String)> {
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

    let mut func_out_sig = BTreeMap::new();
    for (var, type_) in rule.func_vars.iter().zip(rule.func_types.iter()) {
        func_out_sig.insert(var, type_);
    }
    let mut func_vars = Vec::new();
    let mut func_types = Vec::new();
    for (var, type_) in func_out_sig {
        func_vars.push(Ident::new(var.clone()));
        func_types.push(Ident::new(type_.clone()));
    }

    let mut stores = Vec::new();

    for (index, (type_, field_name)) in rule.func_types
        .iter()
        .zip(rule.func_vars.iter())
        .enumerate()
    {
        let field_id = Ident::new(field_name.to_string());
        let store = typed::store(type_, &quote! {self.#field_id});
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

pub fn gen(rule_id: usize, rule: &ir::Rule) -> quote::Tokens {
    let pred_id_k = predicate::names::id(&rule.head_pred);
    let pred_id_k2 = pred_id_k.clone();
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

    let post_tuple = quote! {
        if new {
            let fact = Fact {
                predicate_id: #pred_id_k,
                fact_id: fid
            };
            if let Provenance::Rule {premises, rule_id} = p.clone() {
                for (col, premise) in premises.into_iter().enumerate() {
                    let pred_id = rule_slot_to_pred(rule_id, col);
                    match premise {
                        MergeRef::FactIds(fids) => {
                            for fid in fids {
                                self.fidfids.entry(Fact {
                                    predicate_id: pred_id,
                                    fact_id: fid
                                }).or_insert(Vec::new()).push(fact.clone());
                            }
                        }
                        MergeRef::MetaId(mid) =>
                            self.midfids.entry((pred_id, mid))
                                        .or_insert(Vec::new()).push(fact.clone())
                    }
                }
            }
            productive.push(fact);
        }
        self.purge_mid(#pred_id_k2, broke, fid);
    };

    let tuple_action = match rule.func {
        Some(ref name) => {
            let func = Ident::new(name.clone());
            let view = query::names::result_borrow(&rule.body_query);
            quote! {
                let func_out = #func(&#view::from_tuple(self, tuple.clone()));
                for extra_vars in func_out {
                    let mut tuple = tuple.clone();
                    tuple.extend(&extra_vars.to_tuple(self));
                    let (fid, new, broke) = self.#tuple_name.insert(&[#(#tuple_subs),*], p.clone());
                    #post_tuple
               }
            }
        }
        None => {
            quote! {
                let (fid, new, broke) = self.#tuple_name.insert(&[#(#tuple_subs),*], p.clone());
                #post_tuple
            }
        }
    };

    let rule_id_k = Lit::Int(rule_id as u64, IntTy::Usize);

    quote! {
        pub fn #rule_invoke_name(&mut self) -> Vec<Fact> {
            let mut productive = Vec::new();
            let tuples = self.#query_incr_tuple_name();
            for (tuple, fids) in tuples {
                let p = Provenance::Rule {
                    rule_id: #rule_id_k,
                    premises: fids
                };
                #tuple_action
            }
            productive
        }
    }
}

pub fn preds(
    rule_id: usize,
    query: &ir::Query,
    pred_name_to_id: &BTreeMap<&str, usize>,
) -> quote::Tokens {
    let mut rule_id_ks = Vec::new();
    let mut col_ks = Vec::new();
    let mut pred_id_ks = Vec::new();
    for (col, pred) in query.predicates.iter().enumerate() {
        col_ks.push(Lit::Int(col as u64, IntTy::Usize));
        rule_id_ks.push(Lit::Int(rule_id as u64, IntTy::Usize));
        pred_id_ks.push(Lit::Int(
            pred_name_to_id[pred.as_str()] as u64,
            IntTy::Usize,
        ));
    }
    quote! {
        #((#rule_id_ks, #col_ks) => #pred_id_ks,)*
    }
}
