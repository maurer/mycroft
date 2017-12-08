// The parser! macro spews unbound matches, so disable the lint in this file
#![cfg_attr(feature = "cargo-clippy", allow(unneeded_field_pattern))]
//! Provides parsing functions for the Mycroft language.
use ast::*;
use combine::{any, between, many, not_followed_by, optional, parser, skip_many, try, Parser,
              sep_by1};
use combine::char::{char, digit, letter, newline, spaces, string};
use combine::primitives::{Consumed, Stream};

parser! {
    fn comment[I]()(I) -> ()
        where [I: Stream<Item=char>] {
        (string("//"), skip_many(not_followed_by(newline()).with(any())), newline(), try(spaces()))
            .map(|_| ())
    }
}

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
    fn paren[I]()(I) -> char
        where [I: Stream<Item=char>] {
        char('(').or(char(')'))
    }
}

parser! {
    fn non_paren[I]()(I) -> char
        where [I: Stream<Item=char>] {
        (not_followed_by(paren()), any()).map(|x| x.1)
    }
}

parser! {
    fn nested_paren[I](n: usize)(I) -> String
        where [I: Stream<Item=char>] {
        (many(non_paren()), paren()).then(|data: (String, char)| {
            match data.1 {
                '(' => nested_paren(*n + 1).map(move |sub| data.0.clone() + "(" + &sub).boxed(),
                ')' if *n == 1 => parser(move |p| Ok((data.0.clone(), Consumed::Empty(p)))).boxed(),
                ')' => nested_paren(*n - 1).map(move |sub| data.0.clone() + ")" + &sub).boxed(),
                _ => panic!("Paren-parser returned non-paren character")
            }
        })
    }
}

parser! {
    fn paren_match[I]()(I) -> String
        where [I: Stream<Item=char>] {
        (lex_char('('), nested_paren(1)).map(|x| x.1)
    }
}

parser! {
    // This is less expressive than I'd like, but short of an actual rust parser, it's hard to say
    // "followed by a rust expression", so for now I'm just going to accept an ident, and encourage
    // the use of a const in the module.
    fn match_const[I]()(I) -> Match
        where [I: Stream<Item=char>] {
        (lex_char('~'), qual_ident().or(paren_match())).map(|k| Match::Const(k.1))
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

// TODO: consider more thoroughly dedup with named_fields when syntax is finalized
parser! {
    fn named_fields_match[I]()(I) -> Fields<Match>
        where [I: Stream<Item=char>] {
        let field = (ident().skip(lex_char(':')), match_()).map(|f| {
            NamedField {
                name: f.0,
                val: f.1
            }});
        let shortcut_field = try(field).or(ident().map(|i| NamedField {
            name: i.clone(),
            val: Match::Var(i.clone()),
        }));
        between(lex_char('{'), lex_char('}'),
                sep_by1(shortcut_field, lex_char(','))).map(Fields::Named)
    }
}

parser! {
    fn field_type[I]()(I) -> FieldType
        where [I: Stream<Item=char>] {
        let type_ = ident();
        let agg = lex_char('^').with(ident());
        (type_, optional(agg)).map(|p| FieldType {
            type_: p.0,
            aggregator: p.1
        })
    }
}

parser! {
    fn predicate[I]()(I) -> Predicate
        where [I: Stream<Item=char>] {
        let pred_name = ident();
        let pred_body = ord_fields(field_type()).or(named_fields(field_type()));
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
        let matches = ord_fields(match_()).or(named_fields_match());
        let circ = optional(lex_char('~')).map(|x| x.is_some());
        (circ, pred_name, matches).map(|c| Clause {
            pred_name: c.1,
            matches: c.2,
            circumscribed: c.0
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
        many(try(skip_many(comment()).with(predicate()))),
        many(try(skip_many(comment()).with(query()))),
        many(try(skip_many(comment()).with(rule()))),
        skip_many(comment()))
            .map(|ps| Program {
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
            // A comment (dropped by parser)
            baz{boom: bash, fizz: buzz}
        "#;
        let pred_prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Named(vec![
                        NamedField {
                            name: "boom".to_string(),
                            val: FieldType {
                                type_: "bash".to_string(),
                                aggregator: None,
                            },
                        },
                        NamedField {
                            name: "fizz".to_string(),
                            val: FieldType {
                                type_: "buzz".to_string(),
                                aggregator: None,
                            },
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
            // Between modes
            ?bars: bar(x)
        "#;
        let query_prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
            ],
            queries: vec![
                Query {
                    name: "bars".to_string(),
                    clauses: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                            circumscribed: false,
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
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
                Predicate {
                    name: "out".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "usize".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
            ],
            queries: vec![],
            rules: vec![
                Rule {
                    name: "eq_sig".to_string(),
                    head: Clause {
                        pred_name: "out".to_string(),
                        matches: Fields::Ordered(vec![Match::Const("THREE".to_string())]),
                        circumscribed: false,
                    },
                    body: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                            circumscribed: false,
                        },
                        Clause {
                            pred_name: "baz".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                            circumscribed: false,
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
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
                Predicate {
                    name: "baz".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
                Predicate {
                    name: "out".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "usize".to_string(),
                            aggregator: None,
                        },
                    ]),
                },
            ],
            queries: vec![],
            rules: vec![
                Rule {
                    name: "eq_sig".to_string(),
                    head: Clause {
                        pred_name: "out".to_string(),
                        matches: Fields::Ordered(vec![Match::Var("y".to_string())]),
                        circumscribed: false,
                    },
                    body: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                            circumscribed: false,
                        },
                        Clause {
                            pred_name: "baz".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("x".to_string())]),
                            circumscribed: false,
                        },
                    ],
                    func: Some("usizify".to_string()),
                },
            ],
        };
        assert_eq!(Ok((prog, "")), program().parse(code));
    }

    #[test]
    fn agg() {
        let agg_prog_code = r#"
            bar(bang^bash)
        "#;
        let agg_prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: Some("bash".to_string()),
                        },
                    ]),
                },
            ],
            rules: Vec::new(),
            queries: Vec::new(),
        };
        assert_eq!(Ok((agg_prog, "")), program().parse(agg_prog_code));
    }

    #[test]
    fn circ() {
        let prog_code = r#"
            bar(bang^bash)
            ?circ_query: ~bar(v) & bar(v) & ~bar(v)
        "#;
        let prog = Program {
            predicates: vec![
                Predicate {
                    name: "bar".to_string(),
                    fields: Fields::Ordered(vec![
                        FieldType {
                            type_: "bang".to_string(),
                            aggregator: Some("bash".to_string()),
                        },
                    ]),
                },
            ],
            rules: Vec::new(),
            queries: vec![
                Query {
                    name: "circ_query".to_string(),
                    clauses: vec![
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("v".to_string())]),
                            circumscribed: true,
                        },
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("v".to_string())]),
                            circumscribed: false,
                        },
                        Clause {
                            pred_name: "bar".to_string(),
                            matches: Fields::Ordered(vec![Match::Var("v".to_string())]),
                            circumscribed: true,
                        },
                    ],
                },
            ],
        };
        assert_eq!(Ok((prog, "")), program().parse(prog_code));
    }
}
