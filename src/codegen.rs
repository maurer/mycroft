//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use ir;
use quote;
use syn::{Ident, Lit, IntTy};

mod typed {
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
}

mod predicate {
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
            for (index, (type_, field_name)) in
                pred.types.iter().zip(field_names.clone()).enumerate()
            {
                let store = typed::store(type_, quote! {self.#field_name});
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
}

mod query {
    use ir;
    use syn::{Ident, Lit, IntTy};
    use quote;
    use super::{typed, predicate};
    use std::collections::HashMap;

    pub struct QueryOut {
        pub init: quote::Tokens,
        pub impls: quote::Tokens,
        pub decls: quote::Tokens,
    }

    pub fn store_name(query: &ir::Query) -> Ident {
        Ident::new(format!("query_storage_{}", query.name.to_lowercase()))
    }

    fn func_name(query: &ir::Query) -> Ident {
        Ident::new(format!("query_{}", query.name.to_lowercase()))
    }

    fn incr_func_name(query: &ir::Query) -> Ident {
        Ident::new(format!("query_incr_{}", query.name.to_lowercase()))
    }

    fn result_name(query: &ir::Query) -> Ident {
        Ident::new(format!("{}Result", query.name))
    }

    fn tuple_names(query: &ir::Query, preds: &HashMap<String, ir::Predicate>) -> Vec<Ident> {
        query
            .predicates
            .iter()
            .map(|pred_name| predicate::tuple_name(&preds[pred_name]))
            .collect()
    }

    fn proj_name(id: usize) -> Ident {
        Ident::new(format!("proj_{}", id))
    }

    fn idx_name(id: usize) -> Ident {
        Ident::new(format!("iter_{}", id))
    }

    fn subjoin_mailbox_name(id: usize) -> Ident {
        Ident::new(format!("subjoin_mailbox_{}", id))
    }

    fn subjoin_proj_name(id: usize) -> Ident {
        Ident::new(format!("subjoin_proj_{}", id))
    }

    fn subjoin_name(id: usize) -> Ident {
        Ident::new(format!("subjoin_{}", id))
    }

    fn subjoin_indices_name(id: usize) -> Ident {
        Ident::new(format!("subjoin_indices_{}", id))
    }

    fn subjoin_idx_name(id: usize, sub: usize) -> Ident {
        Ident::new(format!("subjoin_idx_{}_{}", id, sub))
    }

    fn build_projs(
        query: &ir::Query,
        preds: &HashMap<String, ir::Predicate>,
    ) -> (quote::Tokens, Vec<quote::Tokens>) {
        let mut proj_nums = Vec::new();
        let build_projs = query
            .gao
            .iter()
            .enumerate()
            .map(|(pred_id, sub)| {
                let raw_nums = sub.iter()
                    .map(|n| Lit::Int(*n as u64, IntTy::Usize))
                    .collect::<Vec<_>>();
                let nums = quote! { &[#(#raw_nums),*] };
                proj_nums.push(nums.clone());
                let pred_name = predicate::tuple_name(&preds[&query.predicates[pred_id]]);
                let proj_i = proj_name(pred_id);
                quote! {
                    let #proj_i = self.#pred_name.projection(#nums);
                }
            })
            .collect::<Vec<_>>();
        (
            quote! {
                #(#build_projs;)*
            },
            proj_nums,
        )
    }

    fn build_idxs(query: &ir::Query) -> quote::Tokens {
        let build_idxs = query
            .gao
            .iter()
            .enumerate()
            .map(|(pred_id, _)| {
                let proj_i = proj_name(pred_id);
                let iter_i = idx_name(pred_id);
                quote! {
                    let mut #iter_i = #proj_i.skip_iter();
                }
            })
            .collect::<Vec<_>>();
        quote! {
            #(#build_idxs;)*
        }
    }

    fn restricts(query: &ir::Query, preds: &HashMap<String, ir::Predicate>) -> quote::Tokens {
        let mut fields = Vec::new();
        let mut restricts = Vec::new();
        for (qf, v) in query.unify.iter() {
            let clause = Lit::Int(qf.pred_id as u64, IntTy::Usize);
            let field = Lit::Int(qf.field_id as u64, IntTy::Usize);
            let var = Lit::Int(*v as u64, IntTy::Usize);
            fields.push(quote! {
                Field {
                    clause: #clause,
                    field: #field,
                }
            });
            restricts.push(quote! {
                Restrict::Unify(#var)
            });
        }
        for (qf, k) in query.eq.iter() {
            let clause = Lit::Int(qf.pred_id as u64, IntTy::Usize);
            let field = Lit::Int(qf.field_id as u64, IntTy::Usize);
            fields.push(quote! {
                Field {
                    clause: #clause,
                    field: #field,
                }
            });
            let type_ = &preds[&query.predicates[qf.pred_id]].types[qf.field_id];
            let id_k = Ident::new(k.clone());
            let k = typed::store(type_, quote! {#id_k});
            restricts.push(quote! {
                Restrict::Const(#k)
            });
        }
        quote! {
            {
                let mut restricts = HashMap::new();
                #(restricts.insert(#fields, #restricts);)*
                restricts
            }
        }

    }

    fn decls(query: &ir::Query) -> quote::Tokens {
        let result = result_name(query);
        let result2 = result.clone();

        let mut vars = Vec::new();
        let mut types = Vec::new();
        let mut loads = Vec::new();
        for (idx, var) in query.vars.iter().enumerate() {
            vars.push(Ident::new(var.clone()));
            let type_ = &query.types[var];
            types.push(Ident::new(type_.clone()));
            loads.push(typed::load(type_, idx));
        }
        let vars2 = vars.clone();
        quote! {
            pub struct #result {
                #(#vars: #types),*
            }
            impl #result2 {
                fn from_tuple(db: &Database, tuple: Vec<usize>) -> Self {
                    Self {
                        #(#vars2: #loads),*
                    }
                }
            }
        }
    }

    fn gen_push_indices(query: &ir::Query) -> quote::Tokens {
        let iter_is = query
            .gao
            .iter()
            .enumerate()
            .map(|(pred_id, _)| idx_name(pred_id))
            .collect::<Vec<_>>();
        quote! {
            #(indices.push(&mut #iter_is);)*
        }
    }

    fn gen_query(
        query: &ir::Query,
        preds: &HashMap<String, ir::Predicate>,
    ) -> (quote::Tokens, quote::Tokens) {
        let result = result_name(query);
        let result2 = result.clone();

        let (build_projs, proj_nums) = build_projs(query, preds);
        let build_idxs = build_idxs(query);
        let push_indices = gen_push_indices(query);
        let query_func = func_name(query);
        let query_store = store_name(query);
        let query_store2 = query_store.clone();
        let tuples = tuple_names(query, preds);
        let restricts = restricts(query, preds);
        (
            quote! {
            pub fn #query_func(&self) -> Vec<#result> {
                #build_projs
                #build_idxs
                let mut indices: Vec<&mut SkipIterator> = Vec::new();
                #push_indices
                Join::new(indices, &self.#query_store.restricts)
                    .map(|tup| #result2::from_tuple(self, tup)).collect()
            }
        },
            quote! {
                #(db.#tuples.register_projection(#proj_nums);)*
                db.#query_store2.restricts = #restricts;
            },
        )
    }

    fn gen_push_incr_indices(query: &ir::Query, pred_id: usize) -> quote::Tokens {
        let mut push_idxs = Vec::new();
        {
            for sub in 0..query.predicates.len() {
                let subjoin_indices = subjoin_indices_name(pred_id);
                if pred_id == sub {
                    let subjoin_mailbox = subjoin_mailbox_name(pred_id);
                    push_idxs.push(quote! {
                        #subjoin_indices.push(&mut #subjoin_mailbox);
                    })
                } else {
                    let subjoin_idx_name = subjoin_idx_name(pred_id, sub);
                    push_idxs.push(quote! {
                        #subjoin_indices.push(&mut #subjoin_idx_name);
                    })
                }
            }
        }
        quote! {
            #(#push_idxs)*
        }
    }

    fn gen_subjoin_indices(query: &ir::Query, pred_id: usize) -> quote::Tokens {
        let mut build_subjoin_idxs = Vec::new();
        for sub in 0..query.predicates.len() {
            if pred_id == sub {
                continue;
            }
            let subjoin_idx = subjoin_idx_name(pred_id, sub);
            let proj = proj_name(sub);
            build_subjoin_idxs.push(quote! {
                let mut #subjoin_idx = #proj.skip_iter();
            });
        }
        quote! {
            #(#build_subjoin_idxs;)*
        }
    }

    fn gen_incr(
        query: &ir::Query,
        preds: &HashMap<String, ir::Predicate>,
    ) -> (quote::Tokens, quote::Tokens) {
        // TODO: add reordering to subjoins so that they each use their own restrict and put their
        // mailbox as the first index to the joiner. This will likely involve work in the IR as
        // well to generate additional permutation/restriction combos - that logic doesn't belong
        // in the code generator.

        let query_result = result_name(query);
        let query_result2 = query_result.clone();

        let query_incr_func = incr_func_name(query);

        let mut subjoin_names = Vec::new();

        let query_store = query
            .predicates
            .iter()
            .map(|_| store_name(query))
            .collect::<Vec<_>>();

        let tuples = tuple_names(query, preds);

        let (build_base_projs, perms) = build_projs(query, preds);

        let build_all_subjoins = {
            let mut build_subjoins = Vec::new();
            let mut build_subproj = Vec::new();
            for idx in 0..query.predicates.len() {
                let subjoin_proj_name = subjoin_proj_name(idx);
                let subjoin_proj_name2 = subjoin_proj_name.clone();
                let subjoin_mailbox = subjoin_mailbox_name(idx);
                let subjoin_name = subjoin_name(idx);
                let build_subjoin_idxs = gen_subjoin_indices(query, idx);
                let push_idxs = gen_push_incr_indices(query, idx);
                let query_store = store_name(query);
                let query_store2 = query_store.clone();
                let tuple = tuples[idx].clone();
                subjoin_names.push(subjoin_name.clone());
                let subjoin_indices = subjoin_indices_name(idx);
                let subjoin_indices2 = subjoin_indices.clone();
                let idx = Lit::Int(idx as u64, IntTy::Usize);
                build_subproj.push(quote! {
                let #subjoin_proj_name =
                    self.#tuple.mailbox(self.#query_store.mailboxes[#idx]);
            });
                build_subjoins.push(quote! {
                    let mut #subjoin_mailbox = #subjoin_proj_name2.skip_iter();
                    #build_subjoin_idxs
                    let mut #subjoin_indices: Vec<&mut SkipIterator> = Vec::new();
                    #push_idxs
                    let #subjoin_name =
                        Join::new(#subjoin_indices2, &self.#query_store2.restricts);
                });
            }
            quote! {
            #(#build_subproj)*
            #build_base_projs
            #(#build_subjoins)*
        }
        };

        let first_subjoin = subjoin_names[0].clone();
        let rest_subjoin = subjoin_names.into_iter().skip(1).collect::<Vec<_>>();

        (
            quote!{
                pub fn #query_incr_func(&mut self) -> Vec<#query_result> {
                    #build_all_subjoins
                    #first_subjoin#(.chain(#rest_subjoin))*
                        .map(|tup| #query_result2::from_tuple(self, tup)).collect()
                } 
            },
            quote! {
             #(db.#query_store.mailboxes.push(db.#tuples.register_mailbox(#perms));)*
         },
        )
    }

    pub fn gen(query: &ir::Query, preds: &HashMap<String, ir::Predicate>) -> QueryOut {
        let (query_func, query_init) = gen_query(query, preds);
        let (incr_func, incr_init) = gen_incr(query, preds);
        QueryOut {
            decls: decls(query),
            impls: quote! {
                #query_func
                #incr_func
           },
            init: quote! {
                #query_init
                #incr_init
            },
        }
    }
}
/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    use std::collections::HashSet;
    let pred_fact_decls = prog.predicates
        .values()
        .map(predicate::fact)
        .collect::<Vec<_>>();
    let pred_inserts = prog.predicates
        .values()
        .map(predicate::insert)
        .collect::<Vec<_>>();
    let mut query_structs = Vec::new();
    let mut query_funcs = Vec::new();
    let mut query_registrations = Vec::new();
    for gen in prog.queries.values().map(|query| {
        query::gen(query, &prog.predicates)
    })
    {
        query_structs.push(gen.decls);
        query_funcs.push(gen.impls);
        query_registrations.push(gen.init)
    }
    let pred_names = prog.predicates
        .values()
        .map(predicate::tuple_name)
        .collect::<Vec<_>>();
    let pred_names2 = pred_names.clone();

    let mut type_set = HashSet::new();
    for pred in prog.predicates.values() {
        for type_ in pred.types.iter() {
            type_set.insert(type_.clone());
        }
    }
    let data_type_names = type_set
        .iter()
        .filter(|x| !typed::is_small(x))
        .map(|x| typed::name(x))
        .collect::<Vec<_>>();
    let data_type_names2 = data_type_names.clone();
    let type_names = type_set
        .into_iter()
        .filter(|type_| !typed::is_small(type_))
        .map(|type_name| Ident::new(type_name))
        .collect::<Vec<_>>();

    let query_storage_names = prog.queries
        .values()
        .map(query::store_name)
        .collect::<Vec<_>>();
    let query_storage_names2 = query_storage_names.clone();

    let arities = prog.predicates
        .values()
        .map(|pred| Lit::Int(pred.types.len() as u64, IntTy::Usize))
        .collect::<Vec<_>>();
    // TODO add naming feature for program so that mycroft can be invoked multiple times
    // in the same module.
    quote! {
       mod mycroft_program {
            #![allow(unused_imports,dead_code,unused_variables,unused_mut)]
            use mycroft_support::storage::{Tuples, Data};
            use mycroft_support::join::{TrivialIterator, Join, SkipIterator, Field, Restrict};
            use std::collections::HashMap;
            #[derive(Default)]
            struct QueryStorage {
                mailboxes: Vec<usize>,
                restricts: HashMap<Field, Restrict>,
            }
            pub struct Database {
                #(#pred_names: Tuples,)*
                #(#data_type_names: Data<#type_names>,)*
                #(#query_storage_names: QueryStorage,)*
            }
            #(#pred_fact_decls)*
            #(#query_structs)*
            impl Database {
                pub fn new() -> Self {
                    let mut db = Self {
                        #(#pred_names2: Tuples::new(#arities),)*
                        #(#data_type_names2: Data::new(),)*
                        #(#query_storage_names2: QueryStorage::default(),)*
                    };
                    #(#query_registrations)*
                    db
                }
                #(#pred_inserts)*
                #(#query_funcs)*
            }
        }
    }
}
