//! The `ast` module defines the parsed form of the Mycroft language.
mod printers;

/// A `Program` contains all parts of a Mycroft program, and is the basic
/// unit to be handed to the code generator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program {
    /// The list of predicate definitions, defining the different kinds of
    /// facts that may be stored in the database.
    pub predicates: Vec<Predicate>,
    /// The list of available queries: questions that the database will be able to answer
    /// efficiently.
    pub queries: Vec<Query>,
    /// The list of rules to be active on the database when running
    pub rules: Vec<Rule>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// A named field is a predicate field for those using the named style.
pub struct NamedField<T> {
    /// Field name
    pub name: String,
    /// Associated value
    pub val: T,
}

/// `Fields` contains either an ordered list of type names or a list of
/// `NamedField`s. These are encoded separately at this stage to ensure that ordered predicates are
/// only matched against ordered match clauses, and named predicates with named match clauses
/// during translation to IR.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Fields<T> {
    /// List of field associated values, matched by position
    Ordered(Vec<T>),
    /// List of named fields, matched by name
    Named(Vec<NamedField<T>>),
}

/// `FieldType` describes how a particular field in a predicate will be stored and viewed
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldType {
    /// Type of values stored
    pub type_: String,
    /// If `None`, values will be viewed as-is. If `Some(func_name)`, matches against this
    /// predicate may return any subset of matching values, aggregated together via the provided
    /// aggegation function.
    /// Any provided function must be commutative and associative for proper functioning, and have
    /// type (T, T) -> T
    pub aggregator: Option<String>,
}

/// A `Predicate` can essentially be seen as a fact-type declaration. It describes the types of
/// each field of a particular relation, and prescribes how it is to be matched (by index or by
/// name).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// Predicate fields. The associated value is the field type.
    pub fields: Fields<FieldType>,
}

/// A `Query` is a predefined question that can be asked of the database. Appropriate indexes and
/// code to use them will be generated. It is formed as a unification query over several clauses.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Query {
    /// Query name
    pub name: String,
    /// Clauses
    pub clauses: Vec<Clause>,
}

/// A `Clause` is one component of a unification query, specifying a predicate combined with
/// restrictions over each variable
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clause {
    /// The predicate to match against
    pub pred_name: String,
    /// The restrictions over fields
    pub matches: Fields<Match>,
}

/// A `Match` is the specifier for how to unify a particular field
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Match {
    /// Unify the field to the named variable
    Var(String),
    /// Unify the field to the named constant (must be a legal rust expression)
    Const(String),
    /// No restriction
    Unbound,
}

/// A `Rule` is a triggered transition that will instantiate the head and insert it when the body
/// is matched.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rule {
    /// Rule name, to be used in debugging and provenance
    pub name: String,
    /// Clause which must contain as its matches only variables appearing in the body and constants
    /// On successful match of the body, the results will be substituted into the head and realized.
    pub head: Clause,
    /// Set of clauses which must unify to provide input to the head
    pub body: Vec<Clause>,
    /// Function to be called on bound body clause variables to produce head solutions
    pub func: Option<String>,
}
