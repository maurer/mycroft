//! `derivation` handles operations related to walking the provenance graph
use std::collections::HashSet;
use storage::tuple::Provenance;

/// Abstract fact, pre-projection
/// Projected facts are handled in the codegen due to their type depending on the contents of the
/// program.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct Fact {
    /// Index in sorted order of the predicate this fact references
    pub predicate_id: usize,
    /// ID of the referenced fact
    pub fact_id: usize,
}

/// Derivation of a given fact, using only reference ids
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
pub enum RawDerivation {
    /// This fact was in the IDB
    Base {
        /// The fact which was in the IDB
        fact: Fact,
    },
    /// The reasoning behind this has been elided to look only at the top of a proof tree
    Truncated {
        /// The fact which we stopped at
        fact: Fact,
    },
    /// This fact was generated through a rule
    Rule {
        /// The fact being derived at this step
        fact: Fact,
        /// Which rule was applied to derive it, index in sorted order of the rule name this fact
        /// references
        rule_id: usize,
        /// Derivations for each argument of this fact
        sub_derivations: Vec<RawDerivation>,
    },
}

impl RawDerivation {
    /// Gives the maximum depth of the derivation
    pub fn depth(&self) -> usize {
        match *self {
            RawDerivation::Base { .. } => 0,
            RawDerivation::Truncated { .. } => 0,
            RawDerivation::Rule {
                ref sub_derivations,
                ..
            } => 1 + (sub_derivations.iter().map(|d| d.depth()).max().unwrap()),
        }
    }
    /// Returns the derviation of a fact by walking the tuplestores.
    /// Should return `Some(deriv)` for anything currently in the EDB, may return None if either
    /// the justification for a fact is circular, or has been retracted.
    pub fn from_storage<'a, F, G>(
        fact: &Fact,
        tuple_func: &F,
        rule_func: &G,
        spine: HashSet<Fact>,
        tree_depth: usize,
    ) -> Option<RawDerivation>
    where
        F: Fn(usize) -> &'a super::storage::Tuples,
        G: Fn(usize, usize) -> usize,
    {
        if tree_depth == 0 {
            return Some(RawDerivation::Truncated { fact: *fact });
        }
        let tuples = tuple_func(fact.predicate_id);
        let mut min_depth: usize = ::std::usize::MAX;
        let mut out = None;
        'prov_loop: for prov in tuples.get_provenance(fact.fact_id) {
            match *prov {
                // If we're in the idb, that's trivially the shortest derivation
                Provenance::Base => return Some(RawDerivation::Base { fact: fact.clone() }),
                Provenance::Rule {
                    rule_id,
                    ref premises,
                } => {
                    let mut sub_out = Vec::new();
                    let mut local_depth = 0;
                    for (col, premises) in premises.iter().enumerate() {
                        for premise in premises {
                            let sub_fact = Fact {
                                predicate_id: rule_func(rule_id, col),
                                fact_id: *premise,
                            };
                            // If the provenance is circular, ignore it
                            if spine.contains(&sub_fact) {
                                continue 'prov_loop;
                            }
                            let mut sub_spine = spine.clone();
                            sub_spine.insert(sub_fact.clone());
                            if let Some(derivation) = RawDerivation::from_storage(
                                &sub_fact,
                                tuple_func,
                                rule_func,
                                sub_spine,
                                tree_depth - 1,
                            ) {
                                let depth = derivation.depth();
                                local_depth = ::std::cmp::max(depth, local_depth);
                                if local_depth > min_depth {
                                    continue 'prov_loop;
                                }
                                sub_out.push(derivation);
                            } else {
                                continue 'prov_loop;
                            }
                        }
                    }
                    out = Some(RawDerivation::Rule {
                        fact: *fact,
                        rule_id: rule_id,
                        sub_derivations: sub_out,
                    });
                    min_depth = local_depth;
                }
            }
        }
        out
    }
}

/// Derivation of a given fact, expanded
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
pub enum Derivation<F> {
    /// This fact was in the IDB
    Base {
        /// The fact which was in the IDB
        fact: F,
    },
    /// This fact was generated through a rule
    Rule {
        /// The fact being derived at this step
        fact: F,
        /// Which rule was applied to derive it
        rule: &'static str,
        /// Derivations for each argument of this fact
        sub_derivations: Vec<Derivation<F>>,
    },
}

impl<F> Derivation<F> {
    /// Takes a raw derivation and projection functions for facts and rule names, and extracts the
    /// derivation details.
    pub fn from_raw<P, Q>(d: RawDerivation, fact_proj: &P, rule_names: &Q) -> Self
    where
        P: Fn(&Fact) -> F,
        Q: Fn(usize) -> &'static str,
    {
        match d {
            // TODO better final representation for truncation?
            RawDerivation::Base { fact } | RawDerivation::Truncated { fact } => Derivation::Base {
                fact: fact_proj(&fact),
            },
            RawDerivation::Rule {
                fact,
                rule_id,
                sub_derivations,
            } => Derivation::Rule {
                fact: fact_proj(&fact),
                rule: rule_names(rule_id),
                sub_derivations: sub_derivations
                    .into_iter()
                    .map(|d| Derivation::from_raw(d, fact_proj, rule_names))
                    .collect(),
            },
        }
    }
}

use std::fmt::{Display, Formatter, Result};

impl<F: Display> Display for Derivation<F> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.print_indented(f, 0)
    }
}

impl<F: Display> Derivation<F> {
    fn print_indented(&self, f: &mut Formatter, indent: usize) -> Result {
        for _ in 0..indent {
            write!(f, "  ")?
        }
        match *self {
            Derivation::Base { ref fact } => write!(f, "{}.", fact)?,
            Derivation::Rule {
                ref fact,
                ref rule,
                ref sub_derivations,
            } => {
                write!(f, "{} by {},\n", fact, rule)?;
                for sub in sub_derivations {
                    sub.print_indented(f, indent + 1)?;
                }
            }
        }
        Ok(())
    }
}
