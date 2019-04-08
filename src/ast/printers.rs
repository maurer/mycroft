use ast::*;
use std::fmt::{Display, Error, Formatter};

fn sep_by<T: Display>(sep: &str, mult: &[T], f: &mut Formatter) -> Result<(), Error> {
    for (idx, v) in mult.iter().enumerate() {
        write!(f, "{}", v)?;
        if idx != mult.len() - 1 {
            write!(f, "{}", sep)?;
        }
    }
    Ok(())
}

impl<T: Display> Display for NamedField<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {}", self.name, self.val)
    }
}

impl<T: Display> Display for Fields<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Fields::Ordered(ref types_) => {
                write!(f, "(")?;
                sep_by(", ", types_, f)?;
                write!(f, ")")
            }
            Fields::Named(ref fields) => {
                write!(f, "{{")?;
                sep_by(", ", fields, f)?;
                write!(f, "}}")
            }
        }
    }
}

impl Display for Match {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Match::Var(ref var) => write!(f, "{}", var),
            Match::Const(ref k) => write!(f, "~{}", k),
            Match::Unbound => write!(f, "_"),
        }
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}{}", self.pred_name, self.matches)
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}{}", self.name, self.fields)
    }
}

impl Display for FieldType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.type_)?;
        if let Some(ref agg) = self.aggregator {
            write!(f, "^{}", agg)?
        }
        Ok(())
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "?{}: ", self.name)?;
        sep_by(" & ", &self.clauses, f)
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {} <- ", self.name, self.head)?;
        sep_by(" & ", &self.body, f)?;
        if let Some(ref func) = self.func {
            write!(f, " + {}", func)?;
        }
        Ok(())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        sep_by("\n", &self.predicates, f)?;
        writeln!(f)?;
        sep_by("\n", &self.queries, f)?;
        writeln!(f)?;
        sep_by("\n", &self.rules, f)
    }
}
