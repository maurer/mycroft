//! `codegen` contains code related to producing Rust code with the relevant data structures.
//! It would be difficult to avoid generating code due to type-level decisions in terms of what
//! data structures and APIs to provide.
use std::collections::HashMap;
use ir;
use quote;
use syn::{Ident, Lit, IntTy};

fn pred_fields(pred: &ir::Predicate) -> Vec<Ident> {
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

fn predicate_fact(pred: &ir::Predicate) -> quote::Tokens {
    let name = Ident::new(pred.name.clone());
    let types = pred.types
        .iter()
        .cloned()
        .map(Ident::new)
        .collect::<Vec<_>>();
    let names = pred_fields(pred);
    let name2 = name.clone();
    let arity = Lit::Int(pred.types.len() as u64, IntTy::Usize);
    let mut type_stores = Vec::new();
    for (index, (type_, field_name)) in pred.types.iter().zip(names.clone()).enumerate() {
        let store = typed::store(type_, quote! {self.#field_name});
        let index_lit = Lit::Int(index as u64, IntTy::Usize);
        type_stores.push(quote! {
            out[#index_lit] = #store;
        });
    }
    let type_loads = pred.types
        .iter()
        .enumerate()
        .map(|(idx, type_)| typed::load(type_, idx))
        .collect::<Vec<_>>();

    let names2 = names.clone();
    let arity2 = arity.clone();
    quote! {
        pub struct #name {
            #(pub #names: #types),*
        }
        impl #name2 {
            fn from_tuple(db: &Database, tuple: &[usize]) -> Self {
                Self {
                    #(#names2: #type_loads),*
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

fn predicate_insert(pred: &ir::Predicate) -> quote::Tokens {
    let insert_name = Ident::new(format!("insert_{}", pred.name.to_lowercase()));
    let fact_name = Ident::new(pred.name.clone());
    let pred_name = Ident::new(format!("pred_{}", pred.name.to_lowercase()));
    quote! {
        pub fn #insert_name(&mut self, fact: #fact_name) -> usize {
            let tuple = fact.to_tuple(self);
            self.#pred_name.insert(&tuple).0
        }
    }
}

struct QueryOut {
    init: quote::Tokens,
    impls: quote::Tokens,
    decls: quote::Tokens,
}

fn query_gen(query: &ir::Query, preds: &HashMap<String, ir::Predicate>) -> QueryOut {
    let query_name = Ident::new(format!("query_{}", query.name.to_lowercase()));
    let query_incr = Ident::new(format!("query_incr_{}", query.name.to_lowercase()));
    let query_result = Ident::new(format!("{}Result", query.name));
    let query_result2 = query_result.clone();
    let query_result3 = query_result.clone();
    let query_result4 = query_result.clone();
    let query_result5 = query_result.clone();
    let query_result6 = query_result.clone();

    let query_vars = query
        .vars
        .iter()
        .map(|var| Ident::new(var.clone()))
        .collect::<Vec<_>>();
    let query_vars2 = query_vars.clone();
    let local_types = query
        .vars
        .iter()
        .map(|var| query.types[var].clone())
        .collect::<Vec<_>>();
    let query_types = local_types
        .iter()
        .map(|ty| Ident::new(ty.clone()))
        .collect::<Vec<_>>();
    let mut proj_preds = Vec::new();
    let mut proj_nums = Vec::new();
    let build_indices = query
        .gao
        .iter()
        .enumerate()
        .map(|(pred_id, sub)| {
            let raw_nums = sub.iter()
                .map(|n| Lit::Int(*n as u64, IntTy::Usize))
                .collect::<Vec<_>>();
            let nums = quote! { &[#(#raw_nums),*] };
            proj_nums.push(nums.clone());
            let pred_name =
                Ident::new(format!("pred_{}", query.predicates[pred_id].to_lowercase()));
            proj_preds.push(pred_name.clone());
            let proj_i = Ident::new(format!("proj_{}", pred_id));
            let proj_i_2 = proj_i.clone();
            let iter_i = Ident::new(format!("iter_{}", pred_id));
            quote! {
            let #proj_i = self.#pred_name.projection(#nums);
            let mut #iter_i = #proj_i_2.skip_iter();
        }
        })
        .collect::<Vec<_>>();
    let proj_preds2 = proj_preds.clone();
    let proj_nums2 = proj_nums.clone();
    let push_indices = {
        let iter_is = query
            .gao
            .iter()
            .enumerate()
            .map(|(pred_id, _)| Ident::new(format!("iter_{}", pred_id)))
            .collect::<Vec<_>>();
        quote! {
            #(indices.push(&mut #iter_is);)*
        }
    };
    let restricts = {
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
    };
    let type_loads = local_types
        .iter()
        .enumerate()
        .map(|(idx, type_)| typed::load(type_, idx))
        .collect::<Vec<_>>();
    let query_store_base = Ident::new(format!("query_storage_{}", query.name.to_lowercase()));
    let query_store_base2 = query_store_base.clone();
    let query_store = proj_preds
        .iter()
        .map(|_| query_store_base.clone())
        .collect::<Vec<_>>();
    let mut subjoin_names = Vec::new();
    let build_all_subjoins = {
        // TODO: add reordering to subjoins so that they each use their own restrict and put their
        // mailbox as the first index to the joiner
        let mut build_subjoins = Vec::new();
        let mut build_subproj = Vec::new();
        // TODO dedup this against build_indices
        let build_all_projs = {
            let all_projs = query
                .gao
                .iter()
                .enumerate()
                .map(|(pred_id, sub)| {
                    let raw_nums = sub.iter()
                        .map(|n| Lit::Int(*n as u64, IntTy::Usize))
                        .collect::<Vec<_>>();
                    let nums = quote! { &[#(#raw_nums),*] };
                    proj_nums.push(nums.clone());
                    let pred_name =
                        Ident::new(format!("pred_{}", query.predicates[pred_id].to_lowercase()));
                    proj_preds.push(pred_name.clone());
                    let proj_i = Ident::new(format!("proj_{}", pred_id));
                    quote! {
                        let #proj_i = self.#pred_name.projection(#nums);
                    }
                })
                .collect::<Vec<_>>();
            quote! {
                #(#all_projs)*
            }
        };
        for idx in 0..build_indices.len() {
            let subjoin_indices_name = Ident::new(format!("subjoin_indices_{}", idx));
            let subjoin_indices_name2 = subjoin_indices_name.clone();
            let subjoin_proj_name = Ident::new(format!("subjoin_proj_{}", idx));
            let subjoin_proj_name2 = subjoin_proj_name.clone();
            let subjoin_idx_name = Ident::new(format!("subjoin_idx_{}", idx));
            let subjoin_name = Ident::new(format!("subjoin_{}", idx));
            let mut build_subjoin_idxs = Vec::new();
            {
                for sub in 0..build_indices.len() {
                    if idx == sub {
                        continue;
                    }
                    let subjoin_idx_name = Ident::new(format!("subjoin_idx_{}_{}", idx, sub));
                    let proj = Ident::new(format!("proj_{}", sub));
                    build_subjoin_idxs.push(quote! {
                        let mut #subjoin_idx_name = #proj.skip_iter();
                    });
                }
            }
            let mut push_idxs = Vec::new();
            {
                for sub in 0..build_indices.len() {
                    let subjoin_indices_name0 = subjoin_indices_name.clone();
                    if idx == sub {
                        let subjoin_idx_name0 = subjoin_idx_name.clone();
                        push_idxs.push(quote! {
                            #subjoin_indices_name0.push(&mut #subjoin_idx_name0);
                        })
                    } else {
                        let subjoin_idx_name = Ident::new(format!("subjoin_idx_{}_{}", idx, sub));
                        push_idxs.push(quote! {
                            #subjoin_indices_name0.push(&mut #subjoin_idx_name);
                        })
                    }
                }
            }
            let query_store_base_0 = query_store_base.clone();
            let query_store_base_01 = query_store_base.clone();
            let pred_name = Ident::new(format!("pred_{}", query.predicates[idx].to_lowercase()));
            subjoin_names.push(subjoin_name.clone());
            let idx = Lit::Int(idx as u64, IntTy::Usize);
            build_subproj.push(quote! {
                let #subjoin_proj_name =
                    self.#pred_name.mailbox(self.#query_store_base_0.mailboxes[#idx]);
            });
            build_subjoins.push(quote! {
                let mut #subjoin_idx_name = #subjoin_proj_name2.skip_iter();
                #(#build_subjoin_idxs;)*
                let mut #subjoin_indices_name: Vec<&mut SkipIterator> = Vec::new();
                #(#push_idxs;)*
                let #subjoin_name =
                    Join::new(#subjoin_indices_name2, &self.#query_store_base_01.restricts);
            });
        }
        quote! {
            #(#build_subproj)*
            #build_all_projs
            #(#build_subjoins;)*
        }
    };
    let first_subjoin = subjoin_names[0].clone();
    let rest_subjoin = subjoin_names.into_iter().skip(1).collect::<Vec<_>>();
    QueryOut {
        decls: quote! {
        pub struct #query_result {
            #(#query_vars: #query_types),*
        }
        impl #query_result2 {
            fn from_tuple(db: &Database, tuple: Vec<usize>) -> Self {
                Self {
                    #(#query_vars2: #type_loads),*
                }
            }
        }
    },
        impls: quote! {
        pub fn #query_name(&self) -> Vec<#query_result3> {
            #(#build_indices;)*
            let mut indices: Vec<&mut SkipIterator> = Vec::new();
            #push_indices
            Join::new(indices, &self.#query_store_base.restricts)
                .map(|tup| #query_result4::from_tuple(self, tup)).collect()
        }
        pub fn #query_incr(&mut self) -> Vec<#query_result5> {
            #build_all_subjoins
            #first_subjoin#(.chain(#rest_subjoin))*
                .map(|tup| #query_result6::from_tuple(self, tup)).collect()
        }
    },
        init: quote! {
        #(db.#proj_preds.register_projection(#proj_nums);)*
        #(db.#query_store.mailboxes.push(db.#proj_preds2.register_mailbox(#proj_nums2));)*
        db.#query_store_base2.restricts = #restricts;
    },
    }
}

/// Transforms a complete Mycroft program in IR form into code to include in a user program
pub fn program(prog: &ir::Program) -> quote::Tokens {
    use std::collections::HashSet;
    let pred_fact_decls = prog.predicates
        .values()
        .map(predicate_fact)
        .collect::<Vec<_>>();
    let pred_inserts = prog.predicates
        .values()
        .map(predicate_insert)
        .collect::<Vec<_>>();
    let mut query_structs = Vec::new();
    let mut query_funcs = Vec::new();
    let mut query_registrations = Vec::new();
    for gen in prog.queries.values().map(|query| {
        query_gen(query, &prog.predicates)
    })
    {
        query_structs.push(gen.decls);
        query_funcs.push(gen.impls);
        query_registrations.push(gen.init)
    }
    let pred_names = prog.predicates
        .keys()
        .map(|name| Ident::new(format!("pred_{}", name.to_lowercase())))
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
        .map(|query| {
            Ident::new(format!("query_storage_{}", query.name.to_lowercase()))
        })
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
