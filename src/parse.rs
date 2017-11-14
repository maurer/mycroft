// The parser! macro spews unbound matches, so disable the lint in this file
#![cfg_attr(feature = "cargo-clippy", allow(unneeded_field_pattern))]
//! Provides parsing functions for the Mycroft language.
use ast::*;
use combine::{Parser, many, between, sep_by1, try, optional};
use combine::char::{letter, spaces, char, digit, string};
use combine::primitives::Stream;

parser! {
    fn char_seq[I, P, Q](p0: P, p: Q)(I) -> String
        where [I: Stream<Item=char>,
               P: Parser<Input=I, Output=char>,
               Q: Parser<Input=I, Output=char>] {
        (p0, many(p).skip(spaces())).map(|(first, rest): (char, String)| {
            // TODO this is probably not the best way to express this, but it's
            // not performance critical so I can fix it later and just copy for now.
            let mut out = String::new();
            out.push(first);
            out.push_str(rest.as_str());
            out
        })
    }
}

parser! {
    fn ident[I]()(I) -> String
        where [I: Stream<Item=char>] {
        let ident_char = letter().or(digit()).or(char('_'));
        char_seq(letter(), ident_char)
    }
}

parser! {
    fn qual_ident[I]()(I) -> String
        where [I: Stream<Item=char>] {
        let ident_char = letter().or(digit()).or(char('_')).or(char(':'));
        let start_char = letter().or(char(':'));
        char_seq(start_char, ident_char)
    }
}

parser! {
    fn lex_char[I](c: char)(I) -> char
        where [I: Stream<Item=char>] {
        char(*c).skip(spaces())
    }
}

parser! {
    fn lex_str[I](s: &'static str)(I) -> &'static str
        where [I: Stream<Item=char>] {
        string(s).skip(spaces())
    }
}

parser! {
    fn match_[I]()(I) -> Match
        where [I: Stream<Item=char>] {
        match_var().or(match_const()).or(match_unbound())
    }
}

parser! {
    fn match_var[I]()(I) -> Match
        where [I: Stream<Item=char>] {
        ident().map(Match::Var)
    }
}

parser! {
    // This is less expressive than I'd like, but short of an actual rust parser, it's hard to say
    // "followed by a rust expression", so for now I'm just going to accept an ident, and encourage
    // the use of a const in the module.
    fn match_const[I]()(I) -> Match
        where [I: Stream<Item=char>] {
        (lex_char('~'), qual_ident()).map(|k| Match::Const(k.1))
    }
}

parser! {
    fn match_unbound[I]()(I) -> Match
        where [I: Stream<Item=char>] {
        lex_char('_').map(|_| Match::Unbound)
    }
}

parser! {
    fn ord_fields[I, P, O](p: P)(I) -> Fields<O>
        where [I: Stream<Item=char>,
               P: Parser<Input=I, Output=O>] {
        between(lex_char('('), lex_char(')'),
                sep_by1(p, lex_char(','))).map(Fields::Ordered)
    }
}

parser! {
    fn named_fields[I, P, O](p: P)(I) -> Fields<O>
        where [I: Stream<Item=char>,
               P: Parser<Input=I, Output=O>] {
        let field = (ident().skip(lex_char(':')), p).map(|f| {
            NamedField {
                name: f.0,
                val: f.1
            }});
        between(lex_char('{'), lex_char('}'),
                sep_by1(field, lex_char(','))).map(Fields::Named)
    }
}

parser! {
    fn predicate[I]()(I) -> Predicate
        where [I: Stream<Item=char>] {
        let pred_name = ident();
        let pred_body = ord_fields(ident()).or(named_fields(ident()));
        (pred_name, pred_body).map(|p| Predicate {
            name: p.0,
            fields: p.1
        })
    }
}

parser! {
    fn clause[I]()(I) -> Clause
        where [I: Stream<Item=char>] {
        let pred_name = ident();
        let matches = ord_fields(match_()).or(named_fields(match_()));
        (pred_name, matches).map(|c| Clause {
            pred_name: c.0,
            matches: c.1
        })
    }
}

parser! {
    fn query[I]()(I) -> Query
        where [I: Stream<Item=char>] {
        let query_name = ident();
        let query_body = sep_by1(clause(), lex_char('&'));
        (char('?'), query_name, lex_char(':'), query_body).map(|q| Query {
            name: q.1,
            clauses: q.3
        })
    }
}

parser! {
    fn rule[I]()(I) -> Rule
        where [I: Stream<Item=char>] {
        let rule_name = ident();
        let rule_head = clause();
        let rule_body = sep_by1(clause(), lex_char('&'));
        let func = optional((lex_char('+'), qual_ident()).map(|f| f.1));
        (rule_name, lex_char(':'), rule_head, lex_str("<-"), rule_body, func).map(|r| Rule {
            name: r.0,
            head: r.2,
            body: r.4,
            func: r.5
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
        many(try(predicate())), many(try(query())), many(try(rule()))).map(|ps| Program {
            predicates: ps.1,
            queries: ps.2,
            rules: ps.3,
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
        let trivial_prog = Program {
            predicates: vec![],
            queries: vec![],
            rules: vec![],
        };
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
                            val: "bash".to_string(),
                        },
                        NamedField {
                            name: "fizz".to_string(),
                            val: "buzz".to_string(),
                        },
                    ]),
                },
            ],
            queries: vec![],
            rules: vec![],
        };
        assert_eq!(Ok((pred_prog, "")), program().parse(pred_prog_code));
    }
    // A trivial query parses
    #[test]
    fn trivial_query() {
        let query_prog_code = r#"
            bar(bang)
            ?bars: bar(x)
        "#;
        let query_prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
            ],
            queries: vec![
                Query {
                    name: "bars".to_string(),
                    clauses: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                        },
                    ],
                },
            ],
            rules: vec![],
        };
        assert_eq!(Ok((query_prog, "")), program().parse(query_prog_code));
    }

    // No defined queries, defined rule
    #[test]
    fn no_q_yes_r() {
        let code = r#"
            bar(bang)
            baz(bang)
            out(usize)
            eq_sig: out(~THREE) <- bar(x) & baz(x)
        "#;
        let prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
                Predicate {
                    name: "out".to_string(),
                    fields: Fields::Ordered(vec!["usize".to_string()]),
                },
            ],
            queries: vec![],
            rules: vec![
                Rule {
                    name: "eq_sig".to_string(),
                    head: Clause {
                        pred_name: "out".to_string(),
                        matches: Fields::Ordered(vec![Match::Const("THREE".to_string())]),
                    },
                    body: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                        },
                        Clause {
                            pred_name: "baz".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                        },
                    ],
                    func: None,
                },
            ],
        };
        assert_eq!(Ok((prog, "")), program().parse(code));
    }

    #[test]
    fn with_func() {
        let code = r#"
            bar(bang)
            baz(bang)
            out(usize)
            eq_sig: out(y) <- bar(x) & baz(x) + usizify
        "#;
        let prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Ordered(vec!["bang".to_string()]),
                },
                Predicate {
                    name: "out".to_string(),
                    fields: Fields::Ordered(vec!["usize".to_string()]),
                },
            ],
            queries: vec![],
            rules: vec![
                Rule {
                    name: "eq_sig".to_string(),
                    head: Clause {
                        pred_name: "out".to_string(),
                        matches: Fields::Ordered(vec![Match::Var("y".to_string())]),
                    },
                    body: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                        },
                        Clause {
                            pred_name: "baz".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                        },
                    ],
                    func: Some("usizify".to_string()),
                },
            ],
        };
        assert_eq!(Ok((prog, "")), program().parse(code));
    }

}
