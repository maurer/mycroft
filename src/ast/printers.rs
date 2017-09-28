use std::fmt::{Display, Formatter, Error};
use ast::*;

fn comma_sep<T: Display>(mult: &Vec<T>, f: &mut Formatter) -> Result<(), Error> {
    for (idx, v) in mult.iter().enumerate() {
        write!(f, "{}", v)?;
        if idx != mult.len() - 1 {
            write!(f, ", ")?;
        }
    }
    Ok(())
}

impl Display for NamedField {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {}", self.name, self.type_)
    }
}

impl Display for Fields {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Fields::Ordered(ref types_) => {
                write!(f, "(")?;
                comma_sep(types_, f)?;
                write!(f, ")")
            }
            Fields::Named(ref fields) => {
                write!(f, "{{")?;
                comma_sep(fields, f)?;
                write!(f, "}}")
            }
        }
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}{}", self.name, self.fields)
    }
}
