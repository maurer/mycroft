use ir;
use syn::{Ident, Lit, IntTy};
use quote;
use super::{typed, predicate};
use std::collections::HashMap;

pub mod names {
    use ir;
    use syn::Ident;
    use super::super::predicate;
    use codegen::camelize;

    // Name of query local storage
    pub fn store(query: &ir::Query) -> Ident {
        Ident::new(format!("query_storage_{}", query.name.to_lowercase()))
    }

    // Name of the full query function
    pub fn func(query: &ir::Query) -> Ident {
        Ident::new(format!("query_{}", query.name.to_lowercase()))
    }

    // Name of the incremental query function
    pub fn incr_func(query: &ir::Query) -> Ident {
        Ident::new(format!("query_incr_{}", query.name.to_lowercase()))
    }

    // Name of the incremental function producing tuples rather than facts
    pub fn incr_tuple(query_name: &str) -> Ident {
        Ident::new(format!("query_incr_tuple_{}", query_name.to_lowercase()))
    }

    // Name of the query's result type
    pub fn result(query: &ir::Query) -> Ident {
        Ident::new(format!("{}Result", camelize(&query.name)))
    }

    // Name of the query's borrowed result type
    pub fn result_borrow(name: &str) -> Ident {
        Ident::new(format!("{}View", camelize(name)))
    }

    // Name of the phantom to deal with empty borrows
    pub fn borrow_phantom(_query: &ir::Query) -> Ident {
        // TODO: Use query to engage in name collision avoidance
        Ident::new("mycroft_internal_phantom".to_string())
    }

    // Name of the tuple storages needed for this query
    pub fn tuples(query: &ir::Query) -> Vec<Ident> {
        query
            .predicates
            .iter()
            .map(|x| predicate::names::tuple(x))
            .collect()
    }

    // Local variable name for projection (id = which pred)
    pub fn proj(id: usize) -> Ident {
        Ident::new(format!("proj_{}", id))
    }

    // Local variable name for an iterator (id = which pred)
    pub fn idx(id: usize) -> Ident {
        Ident::new(format!("iter_{}", id))
    }

    // Local variable name for a subjoin's mailbox iterator (id = which pred)
    pub fn subjoin_mailbox(id: usize) -> Ident {
        Ident::new(format!("subjoin_mailbox_{}", id))
    }

    // Local variable name for a subjoin's mailbox projection (id = which pred)
    pub fn subjoin_proj(id: usize) -> Ident {
        Ident::new(format!("subjoin_proj_{}", id))
    }

    // Local variable name for a subjoin (id = which pred was mailboxed)
    pub fn subjoin(id: usize) -> Ident {
        Ident::new(format!("subjoin_{}", id))
    }

    // Local variable name for a subjoin's indices (id = which pred was mailboxed)
    pub fn subjoin_indices(id: usize) -> Ident {
        Ident::new(format!("subjoin_indices_{}", id))
    }

    // Local variable name for a subjoin index (id = which pred was mailboxed, sub = which pred
    // this index is of)
    pub fn subjoin_idx(id: usize, sub: usize) -> Ident {
        Ident::new(format!("subjoin_idx_{}_{}", id, sub))
    }
}

pub struct QueryOut {
    // Put this in new()
    pub init: quote::Tokens,
    // Put this in the impl
    pub impls: quote::Tokens,
    // Put this at module scope
    pub decls: quote::Tokens,
}

// Generates all the projections for the predicates used in the query
fn build_projs(query: &ir::Query) -> (quote::Tokens, Vec<quote::Tokens>) {
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
            let pred_name = predicate::names::tuple(&query.predicates[pred_id]);
            let proj_i = names::proj(pred_id);
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

// Assuming the projections have been generated, generates indexes for the query
fn build_idxs(query: &ir::Query) -> quote::Tokens {
    let build_idxs = query
        .gao
        .iter()
        .enumerate()
        .map(|(pred_id, _)| {
            let proj_i = names::proj(pred_id);
            let iter_i = names::idx(pred_id);
            quote! {
                let mut #iter_i = #proj_i.skip_iter();
            }
        })
        .collect::<Vec<_>>();
    quote! {
        #(#build_idxs;)*
    }
}

// Generates an expression which evaluates to the restricts value for the intended join
fn restricts(query: &ir::Query) -> quote::Tokens {
    let mut restricts = Vec::new();
    for row in &query.matches {
        let mut row_out = Vec::new();
        for mr in row {
            match *mr {
                Some(ir::MatchVal::Var(ref v)) => {
                    let var = Lit::Int(*v as u64, IntTy::Usize);
                    row_out.push(quote! {
                        Some(Restrict::Unify(#var))
                    })
                }
                Some(ir::MatchVal::Const(ref k)) => {
                    let k = typed::const_name(k);
                    row_out.push(quote! {
                        Some(Restrict::Const(db.#k))
                    })
                }
                None => row_out.push(quote! {None}),
            }
        }
        restricts.push(quote! {
            vec![#(#row_out),*]
        });
    }
    quote! { vec![#(#restricts),*] }
}

// Generates the structs + from_tuple for our result types
fn decls(query: &ir::Query) -> quote::Tokens {
    let result = names::result(query);
    let result2 = result.clone();

    let result_borrow = names::result_borrow(&query.name);
    let result_borrow2 = result_borrow.clone();

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
    let vars3 = vars.clone();
    let loads2 = loads.clone();
    // Phantom is needed here for query results with an empty variable list
    let borrow_phantom = names::borrow_phantom(query);
    let borrow_phantom2 = borrow_phantom.clone();
    // Need to split into big/small vars/types to avoid &u64 and similar
    let mut big_vars = Vec::new();
    let mut small_vars = Vec::new();
    let mut big_types = Vec::new();
    let mut small_types = Vec::new();
    for var in &query.vars {
        let type_ = &query.types[var];
        let var_name = Ident::new(var.clone());
        let type_name = Ident::new(type_.clone());
        if typed::is_small(type_) {
            small_vars.push(var_name);
            small_types.push(type_name);
        } else {
            big_vars.push(var_name);
            big_types.push(type_name);
        }
    }
    quote! {
        pub struct #result {
            #(pub #vars: #types),*
        }
        impl #result2 {
            fn from_tuple(db: &Database, tuple: Vec<usize>) -> Self {
                Self {
                    #(#vars2: (#loads).clone()),*
                }
            }
        }

        pub struct #result_borrow<'a> {
            #(pub #big_vars: &'a #big_types,)*
            #(pub #small_vars: #small_types,)*
            #borrow_phantom: ::std::marker::PhantomData<&'a ()>
        }
        impl<'a> #result_borrow2<'a> {
            fn from_tuple(db: &'a Database, tuple: Vec<usize>) -> Self {
                Self {
                    #(#vars3: #loads2,)*
                    #borrow_phantom2: ::std::marker::PhantomData
                }
            }
        }
    }
}

// Makes push statements for the variable "indices" for all the normal skipiterators
// Used for full query only
fn gen_push_indices(query: &ir::Query) -> quote::Tokens {
    let iter_is = query
        .gao
        .iter()
        .enumerate()
        .map(|(pred_id, _)| names::idx(pred_id))
        .collect::<Vec<_>>();
    quote! {
        #(indices.push(&mut #iter_is);)*
    }
}

// Make the full query
fn gen_query(query: &ir::Query) -> (quote::Tokens, quote::Tokens) {
    let result = names::result(query);
    let result2 = result.clone();

    let (build_projs, proj_nums) = build_projs(query);
    let build_idxs = build_idxs(query);
    let push_indices = gen_push_indices(query);
    let query_func = names::func(query);
    let query_store = names::store(query);
    let query_store2 = query_store.clone();
    let tuples = names::tuples(query);
    let restricts = restricts(query);
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

// Push the indices for the incremental query for the given predicate
// Targets subjoin_indices(pred_id), and uses the mailbox for the pred_id index rather than a
// normal one.
fn gen_push_incr_indices(query: &ir::Query, pred_id: usize) -> quote::Tokens {
    let mut push_idxs = Vec::new();
    {
        for sub in 0..query.predicates.len() {
            let subjoin_indices = names::subjoin_indices(pred_id);
            if pred_id == sub {
                let subjoin_mailbox = names::subjoin_mailbox(pred_id);
                push_idxs.push(quote! {
                    #subjoin_indices.push(&mut #subjoin_mailbox);
                })
            } else {
                let subjoin_idx_name = names::subjoin_idx(pred_id, sub);
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

// Builds the subjoin indices, assuming that the mailbox + normal projections exist
fn gen_subjoin_indices(query: &ir::Query, pred_id: usize) -> quote::Tokens {
    let mut build_subjoin_idxs = Vec::new();
    for sub in 0..query.predicates.len() {
        if pred_id == sub {
            continue;
        }
        let subjoin_idx = names::subjoin_idx(pred_id, sub);
        let proj = names::proj(sub);
        build_subjoin_idxs.push(quote! {
            let mut #subjoin_idx = #proj.skip_iter();
        });
    }
    quote! {
        #(#build_subjoin_idxs;)*
    }
}

// Builds the incremental query
fn gen_incr(query: &ir::Query) -> (quote::Tokens, quote::Tokens) {
    // TODO: add reordering to subjoins so that they each use their own restrict and put their
    // mailbox as the first index to the joiner. This will likely involve work in the IR as
    // well to generate additional permutation/restriction combos - that logic doesn't belong
    // in the code generator.

    let query_result = names::result(query);
    let query_result2 = query_result.clone();

    let query_incr_func = names::incr_func(query);
    let query_incr_tuple_name = names::incr_tuple(&query.name);
    let query_incr_tuple_name2 = query_incr_tuple_name.clone();

    let mut subjoin_names = Vec::new();

    let query_store = query
        .predicates
        .iter()
        .map(|_| names::store(query))
        .collect::<Vec<_>>();

    let tuples = names::tuples(query);

    let (build_base_projs, perms) = build_projs(query);

    let build_all_subjoins = {
        // Take your mail
        let mut build_subproj = Vec::new();
        // Actually build join objects
        let mut build_subjoins = Vec::new();
        for (idx, tuple) in tuples.iter().enumerate() {
            let subjoin_proj_name = names::subjoin_proj(idx);
            let subjoin_proj_name2 = subjoin_proj_name.clone();

            let subjoin_mailbox = names::subjoin_mailbox(idx);

            let subjoin_name = names::subjoin(idx);
            subjoin_names.push(subjoin_name.clone());

            let query_store = names::store(query);
            let query_store2 = query_store.clone();

            let subjoin_indices = names::subjoin_indices(idx);
            let subjoin_indices2 = subjoin_indices.clone();

            let build_subjoin_idxs = gen_subjoin_indices(query, idx);
            let push_idxs = gen_push_incr_indices(query, idx);

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
        // You must build the mailbox indices (build_subproj) FIRST, because they mutate the
        // storage, and so build_base_projs, which _borrows_ the storage, can't have them borrowed
        // when that hapens.
        quote! {
            #(#build_subproj)*
            #build_base_projs
            #(#build_subjoins)*
        }
    };

    // Split the subjoin up into parts to make it easier to call .chain()
    let first_subjoin = subjoin_names[0].clone();
    let rest_subjoin = subjoin_names.into_iter().skip(1).collect::<Vec<_>>();

    (
        quote! {
            fn #query_incr_tuple_name(&mut self) -> Vec<Vec<usize>> {
                #build_all_subjoins
                #first_subjoin#(.chain(#rest_subjoin))*
                .collect()
            }
            pub fn #query_incr_func(&mut self) -> Vec<#query_result> {
                self.#query_incr_tuple_name2().into_iter().map(|tup| #query_result2::from_tuple(self, tup)).collect()
            }
        },
        quote! {
            #(db.#query_store.mailboxes.push(db.#tuples.register_mailbox(#perms));)*
        },
    )
}

pub fn gen(query: &ir::Query) -> QueryOut {
    let (query_func, query_init) = gen_query(query);
    let (incr_func, incr_init) = gen_incr(query);
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

pub fn consts(query: &ir::Query, preds: &HashMap<String, ir::Predicate>) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (pred_id, row) in query.matches.iter().enumerate() {
        for (field_id, mk) in row.iter().enumerate() {
            if let Some(ir::MatchVal::Const(ref k)) = *mk {
                let type_ = &preds[&query.predicates[pred_id]].types[field_id];
                out.push((k.clone(), type_.to_string()))
            }
        }
    }
    out
}
