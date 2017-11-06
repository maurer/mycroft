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

/// A `Predicate` can essentially be seen as a fact-type declaration. It describes the types of
/// each field of a particular relation, and prescribes how it is to be matched (by index or by
/// name).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// Predicate fields. The associated value is the field type.
    pub fields: Fields<String>,
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
