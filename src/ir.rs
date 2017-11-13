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
            PredicateDefinedTwice(first: Box<ast::Predicate>, second: Box<ast::Predicate>) {
                description("Predicate defined twice."),
                display("{} already defined as {}", second, first),
            }
            #[doc = "A query is defined twice"]
            QueryDefinedTwice(first: Box<ast::Query>, second: Box<ast::Query>) {
                description("Query defined twice."),
                display("{} already defined as {}", second, first),
            }
            #[doc = "A query uses a predicate in its body which is not defined elsewhere"]
            QueryUndefinedPredicate(ast: Box<ast::Query>, name: String) {
                description("Query uses undefined predicate"),
                display("Predicate {:?} not defined for query {}", name, ast)
            }
            #[doc = "A query attempts to unify two fields of different types onto the same \
                     variable"]
            QueryTypeMismatch(ast: Box<ast::Query>, var: String, type_0: String, type_1: String) {
                description("Query unifies variable at two or more types"),
                display("Query {} attempts to unify {} at multiple types, first {} and then {}",
                        ast, var, type_0, type_1)
            }
            #[doc = "A clause tried to match an ordered predicate with the wrong number of fields"]
            ShortClause(pred: Box<super::Predicate>, len: usize) {
                description("Clause had wrong field count when matching an ordered predicate"),
                display("Tried to match {:?} with {} fields", pred, len),
            }
            #[doc = "A clause tried to match with named fields against an ordered predicate or \
                     vice versa"]
            MatchStyle(pred: Box<super::Predicate>) {
                description("Clause matched against with wrong style"),
                display("Tried to match against {:?} with an incompatible style", pred)
            }
            #[doc = "A predicate field was bound twice in the same clause"]
            DoubleBind(pred: Box<super::Predicate>, field: String) {
                description("Clause bound predicate field twice or more"),
                display("Tried to bind {} multiple times in {:?}", field, pred),
            }
            #[doc = "A clause references an undefined predicate field"]
            UndefinedField(pred: Box<super::Predicate>, field: String) {
                description("Clause bound predicate field which is not defined"),
                display("Tried to bind {}, which is not defined in {:?}", field, pred),
            }
            #[doc = "Used an unbound match in a head field"]
            UnboundHeadField(rule: Box<ast::Rule>) {
                description("Used an unbound match in a head field"),
                display("Unbound field in head in rule {}", rule),
            }
            #[doc = "Rule creates fact in undefined predicate"]
            HeadPredUndefined(rule: Box<ast::Rule>) {
                description("Rule creates fact in undefined predicate"),
                display("Head predicate not defined in {}", rule),
            }
            #[doc = "Rule variable does not match the required type in head predicate"]
            HeadTypeMismatch(rule: Box<ast::Rule>, var: String, type_: String) {
                description("Rule variable does not match the type in the head"),
                display("In '{}', '{}' does not have the required type '{}'", rule, var, type_),
            }
            #[doc = "Two rules were defined with the same name"]
            RuleDefinedTwice(rule0: Box<ast::Rule>, rule1: Box<ast::Rule>) {
                description("Two rules were defined with the same name"),
                display("One of '{}' and '{}' must be renamed.", rule0, rule1),
            }
            #[doc = "Variable not found in search area."]
            VarNotFound(hay: Vec<String>, needle: String) {
                description("Variable not found when looking up definition"),
                display("{:?} not found in {:?}", needle, hay),
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
                for field in sorted_fields {
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
/// All `QueryField` values are relative to the ordering given by the GAO
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
    /// Indexed by query, then field, what restrictions are on that coordinate
    pub matches: Vec<Vec<Option<MatchVal>>>,
    /// Map between variables and their types
    pub types: HashMap<String, String>,
    /// For each predicate, how it should be projected in the ordering
    pub gao: Vec<Vec<usize>>,
}

/// Values usable to describe a restriction or instantiation of a query variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchVal {
    /// nth variable returned by the generated query
    Var(usize),
    /// Constant specified by the identifier
    Const(String),
}

/// IR Rule Representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    /// Rule name
    pub name: String,
    /// Rule AST, for error reporting
    pub ast: ast::Rule,
    /// Name of generated query, returned alongside production of rule, to represent body
    /// condition
    pub body_query: String,
    /// Name of predicate to be used for the head
    pub head_pred: String,
    /// List of variables or constants to be used instantiating the head
    pub head_vals: Vec<MatchVal>,
    /// Function to call to fill in the rest of the variables
    pub func: Option<String>,
    /// List of variables which should be defined by the function
    pub func_vars: Vec<String>,
}

fn find_var(hay: &[String], needle: &str) -> Result<usize> {
    for (idx, s) in hay.iter().enumerate() {
        if s == needle {
            return Ok(idx);
        }
    }
    Err(
        ErrorKind::VarNotFound(hay.to_vec(), needle.to_string()).into(),
    )
}

impl Rule {
    fn from_ast(
        ast: ast::Rule,
        preds: &HashMap<String, Predicate>,
        queries: &mut HashMap<String, Query>,
    ) -> Result<Self> {
        // Generate a fake ast to make a query out of
        let query_name = format!("mycroft_internal_rule_{}", ast.name);
        // We can do collision avoidance later, but for now, if you're using double
        // underscores on your query name, you're probably just sabotaging yourself
        assert!(!queries.contains_key(&query_name));
        let query_ast = ast::Query {
            name: query_name.clone(),
            clauses: ast.body.clone(),
        };
        let query = Query::from_ast(query_ast, preds)?;
        let head_pred = ast.head.pred_name.clone();
        let pred = match preds.get(&head_pred) {
            Some(pred) => pred,
            None => return Err(ErrorKind::HeadPredUndefined(Box::new(ast)).into()),
        };
        let mut head_vals = Vec::new();
        let mut func_vars = Vec::new();
        let mut idxs = idx_form(pred, &ast.head.matches)?;
        idxs.sort_by_key(|x| x.0);
        for (head_field, match_) in idxs {
            head_vals.push(match match_ {
                ast::Match::Const(ref k) => MatchVal::Const(k.clone()),
                ast::Match::Var(ref v) => {
                    match find_var(&query.vars, v).chain_err(|| "Head clause var lookup") {
                        Ok(var) => {
                            if query.types[v] != pred.types[head_field] {
                                return Err(
                                    ErrorKind::HeadTypeMismatch(
                                        Box::new(ast),
                                        v.clone(),
                                        query.types[v].clone(),
                                    ).into(),
                                );
                            }
                            MatchVal::Var(var)
                        }
                        Err(_) if ast.func.is_some() => {
                            // The variable is undefined, and should be defined by the function
                            let var = func_vars.len() + query.vars.len();
                            func_vars.push(v.to_string());
                            MatchVal::Var(var)
                        }
                        Err(e) => return Err(e),
                    }
                }
                ast::Match::Unbound => {
                    return Err(ErrorKind::UnboundHeadField(Box::new(ast)).into())
                }
            });
        }
        queries.insert(query_name.clone(), query);
        Ok(Rule {
            name: ast.name.clone(),
            func: ast.func.clone(),
            func_vars: func_vars,
            ast: ast,
            body_query: query_name,
            head_pred: head_pred,
            head_vals: head_vals,
        })
    }
}

fn idx_form<T: Clone>(pred: &Predicate, fields: &ast::Fields<T>) -> Result<Vec<(usize, T)>> {
    // TODO: Due to lack of AST, error reporting won't be super clean out of this section
    // Once language is more filled out, figure out what extra information to pass in here
    match *fields {
        ast::Fields::Ordered(ref v) => {
            if pred.names.is_some() {
                return Err(ErrorKind::MatchStyle(Box::new(pred.clone())).into());
            }
            if v.len() != pred.types.len() {
                return Err(
                    ErrorKind::ShortClause(Box::new(pred.clone()), v.len()).into(),
                );
            }
            Ok(v.iter().cloned().enumerate().collect())
        }
        ast::Fields::Named(ref nm) => {
            if pred.names.is_none() {
                return Err(ErrorKind::MatchStyle(Box::new(pred.clone())).into());
            }
            let mut seen_fields: HashSet<String> = HashSet::new();
            let mut out = Vec::new();
            for &ast::NamedField { ref name, ref val } in nm.iter() {
                if seen_fields.contains(name) {
                    return Err(
                        ErrorKind::DoubleBind(Box::new(pred.clone()), name.clone()).into(),
                    );
                }
                seen_fields.insert(name.clone());
                // This ugly bit of code just finds the index of the name present in the named
                // field in the predicate. It's encoded as a 1 or 0 element vector based on whether
                // it was found.
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
                if fields.is_empty() {
                    return Err(
                        ErrorKind::UndefinedField(Box::new(pred.clone()), name.clone()).into(),
                    );
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

// Moves a QueryField in base form to one after GAO permutation to allow it to be used to generate
// a Restrict
fn permute(gao: &[Vec<usize>], qf: &QueryField) -> QueryField {
    for (idx, field) in gao[qf.pred_id].iter().enumerate() {
        if qf.field_id == *field {
            return QueryField {
                pred_id: qf.pred_id,
                field_id: idx,
            };
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
                    ErrorKind::QueryUndefinedPredicate(Box::new(ast), clause.pred_name.clone())
                        .into(),
                );
            }
            predicates.push(clause.pred_name.clone());
            let pred = &preds[&clause.pred_name];
            for (var_idx, match_) in idx_form(pred, &clause.matches)? {
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
                        let var_type = types.entry(s.clone()).or_insert_with(|| qf_type.clone());
                        if qf_type != var_type {
                            return Err(
                                ErrorKind::QueryTypeMismatch(
                                    Box::new(ast),
                                    s.clone(),
                                    var_type.clone(),
                                    qf_type.clone(),
                                ).into(),
                            );
                        }
                        pre_unify.entry(s.clone()).or_insert_with(Vec::new).push(qf);
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
        for &(ref qf, _) in &pre_eq {
            pre_gao.push(qf.clone());
        }
        // Then, add unifications, grouped
        for var in &vars {
            let qfs = &pre_unify[var];
            for qf in qfs.iter() {
                pre_gao.push(qf.clone());
            }
        }
        // Flatten to permutations
        let mut gao = Vec::new();
        for i in 0..predicates.len() {
            let mut entry = Vec::new();
            for qf in &pre_gao {
                if qf.pred_id == i {
                    entry.push(qf.field_id);
                }
            }
            gao.push(entry);
        }

        // Translate pre_unify and pre_eq via the gao
        let mut unify = HashMap::new();
        for (var, qfs) in &pre_unify {
            let var_id = var_map[var];
            for qf in qfs {
                unify.insert(permute(&gao, qf), var_id);
            }
        }
        let mut eq = HashMap::new();
        for &(ref qf, ref k) in &pre_eq {
            eq.insert(permute(&gao, qf), k.clone());
        }

        // Flatten the map for consumption by Join
        let mut matches = Vec::new();
        for (pid, fid_order) in gao.iter().enumerate() {
            let mut row = Vec::new();
            for fid in 0..fid_order.len() {
                let qf = QueryField {
                    pred_id: pid,
                    field_id: fid,
                };
                if let Some(var) = unify.get(&qf) {
                    row.push(Some(MatchVal::Var(*var)));
                    continue;
                }
                if let Some(k) = eq.get(&qf) {
                    row.push(Some(MatchVal::Const(k.clone())));
                    continue;
                }
                row.push(None);
            }
            matches.push(row);
        }

        Ok(Query {
            name: name,
            ast: ast,
            predicates: predicates,
            vars: vars,
            matches: matches,
            types: types,
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
    /// Map from rule name to IR rule
    pub rules: HashMap<String, Rule>,
}

impl Program {
    /// Generate a program IR from an AST
    pub fn from_ast(ast: ast::Program) -> Result<Self> {
        //TODO: this is repetative, dedup it
        let mut predicates: HashMap<String, Predicate> = HashMap::new();
        for ast_pred in ast.predicates {
            let ir_pred = Predicate::from_ast(ast_pred);
            if predicates.contains_key(&ir_pred.name) {
                let first = predicates.remove(&ir_pred.name).unwrap().ast;
                let second = ir_pred.ast;
                return Err(
                    ErrorKind::PredicateDefinedTwice(Box::new(first), Box::new(second)).into(),
                );
            }
            predicates.insert(ir_pred.name.clone(), ir_pred);
        }

        let mut queries: HashMap<String, Query> = HashMap::new();
        for ast_query in ast.queries {
            let ir_query = Query::from_ast(ast_query, &predicates)?;
            if queries.contains_key(&ir_query.name) {
                let first = queries.remove(&ir_query.name).unwrap().ast;
                let second = ir_query.ast;
                return Err(
                    ErrorKind::QueryDefinedTwice(Box::new(first), Box::new(second)).into(),
                );
            }
            queries.insert(ir_query.name.clone(), ir_query);
        }

        let mut rules: HashMap<String, Rule> = HashMap::new();
        for ast_rule in ast.rules {
            let ir_rule = Rule::from_ast(ast_rule, &predicates, &mut queries)?;
            if rules.contains_key(&ir_rule.name) {
                let first = rules.remove(&ir_rule.name).unwrap().ast;
                let second = ir_rule.ast;
                return Err(
                    ErrorKind::RuleDefinedTwice(Box::new(first), Box::new(second)).into(),
                );
            }
            rules.insert(ir_rule.name.clone(), ir_rule);
        }

        Ok(Program {
            predicates: predicates,
            queries: queries,
            rules: rules,
        })
    }
}
