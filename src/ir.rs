//! The IR module describes the internal representation of the program.
//! Transformation into the IR will also check the AST for validity, so errors may be returned in
//! some cases.

use std::collections::{HashMap, HashSet};

use ast;

/// `error-chain`-generated error module for the checks occurring during transformation from
/// AST into IR.
pub mod error {
    use ast;
    error_chain! {
        errors {
            #[doc = "A predicate is defined twice"]
            PredicateDefinedTwice(first: ast::Predicate, second: ast::Predicate) {
                description("Predicate defined twice."),
                display("{} already defined as {}", second, first),
            }
            #[doc = "A query is defined twice"]
            QueryDefinedTwice(first: ast::Query, second: ast::Query) {
                description("Query defined twice."),
                display("{} already defined as {}", second, first),
            }
            #[doc = "A query uses a predicate in its body which is not defined elsewhere"]
            QueryUndefinedPredicate(ast: ast::Query, name: String) {
                description("Query uses undefined predicate"),
                display("Predicate {:?} not defined for query {}", name, ast)
            }
            #[doc = "A query attempts to unify two fields of different types onto the same variable"]
            QueryTypeMismatch(ast: ast::Query, var: String, type_0: String, type_1: String) {
                description("Query unifies variable at two or more types"),
                display("Query {} attempts to unify {} at multiple types, first {} and then {}", ast, var, type_0, type_1)
            }
            #[doc = "A clause tried to match an ordered predicate with the wrong number of fields"]
            ShortClause(pred: super::Predicate, len: usize) {
                description("Clause had wrong field count when matching an ordered predicate"),
                display("Tried to match {:?} with {} fields", pred, len),
            }
            #[doc = "A clause tried to match with named fields against an ordered predicate or vice versa"]
            MatchStyle(pred: super::Predicate) {
                description("Clause matched against with wrong style"),
                display("Tried to match against {:?} with an incompatible style", pred)
            }
            #[doc = "A predicate field was bound twice in the same clause"]
            DoubleBind(pred: super::Predicate, field: String) {
                description("Clause bound predicate field twice or more"),
                display("Tried to bind {} multiple times in {:?}", field, pred),
            }
            #[doc = "A clause references an undefined predicate field"]
            UndefinedField(pred: super::Predicate, field: String) {
                description("Clause bound predicate field which is not defined"),
                display("Tried to bind {}, which is not defined in {:?}", field, pred),
            }
        }
    }
}
use self::error::*;

/// IR Predicate Representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// AST of the predicate, for use in error reporting
    pub ast: ast::Predicate,
    /// Field names, if they exist. If this field is `Some`, the vector length will be equal to
    /// that of `types_`
    pub names: Option<Vec<String>>,
    /// Types of the predicate fields
    pub types: Vec<String>,
}

impl Predicate {
    /// Transforms predicate AST into predicate IR
    pub fn from_ast(ast_pred: ast::Predicate) -> Self {
        let name = ast_pred.name.clone();
        let (names, types) = match ast_pred.fields {
            ast::Fields::Ordered(ref types_) => (None, types_.clone()),
            ast::Fields::Named(ref fields) => {
                let mut names = Vec::new();
                let mut types = Vec::new();
                let mut sorted_fields: Vec<_> = fields.clone();
                sorted_fields.sort();
                for field in sorted_fields.into_iter() {
                    names.push(field.name);
                    types.push(field.val);
                }
                (Some(names), types)
            }
        };
        Predicate {
            name: name,
            ast: ast_pred,
            names: names,
            types: types,
        }
    }
}

/// Rperesentation of a specific field in a query
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QueryField {
    /// Which predicate in the query this projection is on
    pub pred_id: usize,
    /// Which field to project out on
    pub field_id: usize,
}

/// IR Query Representation
/// All QueryField values are relative to the ordering given by the GAO
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query {
    /// Predicate name
    pub name: String,
    /// AST of the predicate, for use in error reporting
    pub ast: ast::Query,
    /// List of predicates we are joining across
    pub predicates: Vec<String>,
    /// Map between variable numbers and names
    pub vars: Vec<String>,
    /// Map between fields and what variable they must unify with
    pub unify: HashMap<QueryField, usize>,
    /// Map between variables and their types
    pub types: HashMap<String, String>,
    /// Equality constraints from a field to a value
    pub eq: HashMap<QueryField, String>,
    /// For each predicate, how it should be projected in the ordering
    pub gao: Vec<Vec<usize>>,
}

fn idx_form<T: Clone>(pred: &Predicate, fields: &ast::Fields<T>) -> Result<Vec<(usize, T)>> {
    // TODO: Due to lack of AST, error reporting won't be super clean out of this section
    // Once language is more filled out, figure out what extra information to pass in here
    match *fields {
        ast::Fields::Ordered(ref v) => {
            if pred.names.is_some() {
                return Err(ErrorKind::MatchStyle(pred.clone()).into());
            }
            if v.len() != pred.types.len() {
                return Err(ErrorKind::ShortClause(pred.clone(), v.len()).into());
            }
            Ok((v.iter().cloned().enumerate().collect()))
        }
        ast::Fields::Named(ref nm) => {
            if pred.names.is_none() {
                return Err(ErrorKind::MatchStyle(pred.clone()).into());
            }
            let mut seen_fields: HashSet<String> = HashSet::new();
            let mut out = Vec::new();
            for &ast::NamedField { ref name, ref val } in nm.iter() {
                if seen_fields.contains(name) {
                    return Err(ErrorKind::DoubleBind(pred.clone(), name.clone()).into());
                }
                seen_fields.insert(name.clone());
                let fields: Vec<_> = pred.names
                    .iter()
                    .map(|ns| {
                        ns.iter()
                            .enumerate()
                            .filter(|&(_, v)| v == name)
                            .map(|x| x.0)
                            .collect::<Vec<_>>()
                    })
                    .next()
                    .unwrap();
                if fields.len() == 0 {
                    return Err(ErrorKind::UndefinedField(pred.clone(), name.clone()).into());
                }
                // This invariant should be handled in predicate construction, but it's cheap to
                // check.
                assert!(!(fields.len() > 1));
                out.push((fields[0], val.clone()));
            }
            Ok(out)
        }
    }
}

fn permute(gao: &Vec<Vec<usize>>, qf: &QueryField) -> QueryField {
    for (idx, field) in gao[qf.pred_id].iter().enumerate() {
        if qf.field_id == *field {
            return QueryField {
                pred_id: qf.pred_id,
                field_id: idx
            }
        }
    }
    panic!("GAO did not contain an entry for the provided query field")
}

impl Query {
    fn from_ast(ast: ast::Query, preds: &HashMap<String, Predicate>) -> Result<Self> {
        let name = ast.name.clone();
        let mut predicates = Vec::new();
        let mut pre_unify = HashMap::new();
        let mut types: HashMap<String, String> = HashMap::new();
        let mut pre_eq = Vec::new();
        let mut vars = Vec::new();
        let mut var_map = HashMap::new();
        for (idx, clause) in ast.clauses.clone().iter().enumerate() {
            if !preds.contains_key(&clause.pred_name) {
                return Err(
                    ErrorKind::QueryUndefinedPredicate(ast, clause.pred_name.clone()).into(),
                );
            }
            predicates.push(clause.pred_name.clone());
            let pred = &preds[&clause.pred_name];
            for (var_idx, match_) in idx_form(&pred, &clause.matches)? {
                let qf = QueryField {
                    pred_id: idx,
                    field_id: var_idx,
                };
                match match_ {
                    ast::Match::Var(ref s) => {
                        let qf_type = &pred.types[var_idx];
                        if !types.contains_key(s) {
                            var_map.insert(s.clone(), vars.len());
                            vars.push(s.clone());
                        }
                        let var_type = types.entry(s.clone()).or_insert(qf_type.clone());
                        if qf_type != var_type {
                            return Err(
                                ErrorKind::QueryTypeMismatch(
                                    ast,
                                    s.clone(),
                                    var_type.clone(),
                                    qf_type.clone(),
                                ).into(),
                            );
                        }
                        pre_unify.entry(s.clone()).or_insert(vec![]).push(qf);
                    }
                    ast::Match::Const(ref k) => {
                        pre_eq.push((qf, k.clone()));
                    }
                    ast::Match::Unbound => (),
                }
            }
        }
        
        // Generate the GAO
        let mut pre_gao = Vec::new();
        // First, any constant terms
        for &(ref qf, _) in pre_eq.iter() {
            pre_gao.push(qf.clone());
        }
        // Then, add unifications, grouped
        for var in vars.iter() {
            let qfs = &pre_unify[var];
            for qf in qfs.iter() {
                pre_gao.push(qf.clone());
            }
        }
        // Flatten to permutations
        let mut gao = Vec::new();
        for i in 0..predicates.len() {
            let mut entry = Vec::new();
            for qf in pre_gao.iter() {
                if qf.pred_id == i {
                    entry.push(qf.field_id);
                }
            }
            gao.push(entry);
        }

        // Translate pre_unify and pre_eq via the gao
        let mut unify = HashMap::new();
        for (var, qfs) in pre_unify.iter() {
            let var_id = var_map[var];
            for qf in qfs {
                unify.insert(permute(&gao, qf), var_id);
            }
        }
        let mut eq = HashMap::new();
        for &(ref qf, ref k) in pre_eq.iter() {
            eq.insert(permute(&gao, qf), k.clone());
        }

        Ok(Query {
            name: name,
            ast: ast,
            predicates: predicates,
            vars: vars,
            unify: unify,
            types: types,
            eq: eq,
            gao: gao,
        })
    }
}

/// IR Program representation
/// This is the structure to be handed off to the code generator.
pub struct Program {
    /// Map from predicate name to IR predicate
    pub predicates: HashMap<String, Predicate>,
    /// Map from query name to IR query
    pub queries: HashMap<String, Query>,
}

impl Program {
    /// Generate a program IR from an AST
    pub fn from_ast(ast: ast::Program) -> Result<Self> {
        let mut predicates: HashMap<String, Predicate> = HashMap::new();
        for ast_pred in ast.predicates {
            let ir_pred = Predicate::from_ast(ast_pred);
            if predicates.contains_key(&ir_pred.name) {
                let first = predicates.remove(&ir_pred.name).unwrap().ast;
                let second = ir_pred.ast;
                return Err(ErrorKind::PredicateDefinedTwice(first, second).into());
            }
            predicates.insert(ir_pred.name.clone(), ir_pred);
        }

        let mut queries: HashMap<String, Query> = HashMap::new();
        for ast_query in ast.queries {
            let ir_query = Query::from_ast(ast_query, &predicates)?;
            if queries.contains_key(&ir_query.name) {
                let first = queries.remove(&ir_query.name).unwrap().ast;
                let second = ir_query.ast;
                return Err(ErrorKind::QueryDefinedTwice(first, second).into());
            }
            queries.insert(ir_query.name.clone(), ir_query);
        }

        Ok(Program {
            predicates: predicates,
            queries: queries,
        })
    }
}
