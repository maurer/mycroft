//! Provides parsing functions for the Mycroft language.
use ast::*;
use combine::{Parser, many, between, sep_by};
use combine::char::{letter, spaces, char, digit};
use combine::primitives::Stream;

parser! {
    fn ident[I]()(I) -> String
        where [I: Stream<Item=char>] {
        let ident_char = letter().or(digit());
        // TODO this is probably not the best way to express this, but it's
        // not performance critical so I can fix it later  and just copy for now.
        (letter(), many(ident_char).skip(spaces())).map(|(first, rest): (char, String)| {
            let mut out = String::new();
            out.push(first);
            out.push_str(rest.as_str());
            out
        })
    }
}

parser! {
    fn lex_char[I](c: char)(I) -> char
        where [I: Stream<Item=char>] {
        char(*c).skip(spaces())
    }
}

parser! {
    fn ord_fields[I]()(I) -> Fields
        where [I: Stream<Item=char>] {
        between(lex_char('('), lex_char(')'),
                sep_by(ident(), lex_char(','))).map(|x| Fields::Ordered(x))
    }
}

parser! {
    fn named_fields[I]()(I) -> Fields
        where [I: Stream<Item=char>] {
        let field = (ident().skip(lex_char(':')), ident()).map(|f| {
            NamedField {
                name: f.0,
                type_: f.1
            }});
        between(lex_char('{'), lex_char('}'),
                sep_by(field, lex_char(','))).map(|x| Fields::Named(x))
    }
}

parser! {
    fn predicate[I]()(I) -> Predicate
        where [I: Stream<Item=char>] {
        let pred_name = ident();
        let pred_body = ord_fields().or(named_fields());
        (pred_name, pred_body).map(|p| Predicate {
            name: p.0,
            fields: p.1
        })
    }
}

parser! {
/// `program` will return a combine parser constructor that will parse legal Mycroft programs.
/// To parse with it:
/// ```
/// use combine::{Parser, Result};
/// use mycroft::parse::program;
/// let prog_code = r#"
///         bar(bang)
///         baz{boom: bash, fizz: buzz}
///         "#;
/// let result = program().parse(prog_code);
/// ```
/// The parse result will be `Ok((p : Program, extra: &str))` if the parse was successful, and a
/// combine error if not. Errors may be poor quality until 1.0.
    pub fn program[I]()(I) -> Program
        where [I: Stream<Item=char>] {
            (spaces(),
             many(predicate())).map(|ps| Program {
                predicates: ps.1
            })
        }
}

#[cfg(test)]
mod test {
    use super::program;
    use ast::*;
    use combine::Parser;

    // Empty program parses
    #[test]
    fn trivial() {
        let trivial_prog_code = "";
        let trivial_prog = Program { predicates: vec![] };
        assert_eq!(Ok((trivial_prog, "")), program().parse(trivial_prog_code));
    }

    // Both types of predicates parse
    #[test]
    fn preds() {
        let pred_prog_code = r#"
            bar(bang)
            baz{boom: bash, fizz: buzz}
        "#;
        let pred_prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Named(vec![
                        NamedField {
                            name: "boom".to_string(),
                            type_: "bash".to_string(),
                        },
                        NamedField {
                            name: "fizz".to_string(),
                            type_: "buzz".to_string(),
                        },
                    ]),
                },
            ],
        };
        assert_eq!(Ok((pred_prog, "")), program().parse(pred_prog_code));
    }
}
