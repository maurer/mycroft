//! The `ast` module defines the parsed form of the Mycroft language.

/// A `Program` contains all parts of a Mycroft program, and is the basic
/// unit to be handed to the code generator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program {
    /// The list of predicate definitions, defining the different kinds of
    /// facts that may be stored in the database.
    pub predicates: Vec<Predicate>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// A named field is a predicate field for those using the named style.
pub struct NamedField {
    /// Field name
    pub name: String,
    /// Field type
    pub type_: String,
}

/// `Fields` contains either an ordered list of type names or a list of
/// `NamedField`s. These are encoded separately at this stage to ensure that ordered predicates are
/// only matched against ordered match clauses, and named predicates with named match clauses
/// during translation to IR.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Fields {
    /// List of type names, matched by position
    Ordered(Vec<String>),
    /// List of named fields, matched by name
    Named(Vec<NamedField>),
}

/// A `Predicate` can essentially be seen as a fact-type declaration. It describes the types of
/// each field of a particular relation, and prescribes how it is to be matched (by index or by
/// name).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// Predicate fields
    pub fields: Fields,
}
