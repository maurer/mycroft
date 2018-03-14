#[macro_use]
extern crate log;
extern crate mycroft;
#[macro_use]
extern crate mycroft_macros;
extern crate mycroft_support;

type Vu16 = Vec<u16>;
const THREE: u64 = 3;
const FOUR: u64 = 4;
const SEVEN: i32 = 7;
mycroft_program!(
    r#"
Bar(u64)
Baz{boom: i32, fizz: Vu16, bash: u64}
Ordered(u64)
Named {out_var: Vu16}
JoinOut {join_out: Vu16, idx: u64}
?check_ordered: Ordered(x)
?check_named: Named {out_var: out_var}
?check_join: JoinOut {join_out: join_out, idx: idx}
?gen_check: Bar(~std::u64::MAX)
?gen_check_expr: Bar(~(3 + 7))
ordered_rule: Ordered(~FOUR) <- Bar(~THREE)
named_rule: Named{out_var: out_var} <- Baz{boom: ~SEVEN, fizz: out_var}
join_rule: JoinOut {join_out: join_out, idx: idx} <- Bar(idx) & Baz{fizz: join_out, bash: idx}
"#
);

#[test]
fn rule_invoke() {
    use mycroft_program::{Bar, Baz, Database};
    let mut db = Database::new();
    db.insert_bar(Bar { arg0: 3 });
    db.insert_bar(Bar { arg0: 42 });
    db.insert_bar(Bar { arg0: 3 });
    assert_eq!(db.query_check_ordered().len(), 0);
    assert!(!db.rule_invoke_ordered_rule().is_empty());
    assert_eq!(db.query_check_ordered().len(), 1);
    db.insert_baz(Baz {
        boom: 2,
        fizz: vec![3, 4],
        bash: 20,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 20,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 3,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![5, 46],
        bash: 42,
    });
    assert_eq!(db.query_check_named().len(), 0);
    assert!(!db.rule_invoke_named_rule().is_empty());
    assert_eq!(db.query_check_named().len(), 2);
    assert_eq!(db.query_check_join().len(), 0);
    assert!(!db.rule_invoke_join_rule().is_empty());
    assert_eq!(db.query_check_join().len(), 2);
    assert!(db.rule_invoke_ordered_rule().is_empty());
    assert!(db.rule_invoke_named_rule().is_empty());
    assert!(db.rule_invoke_join_rule().is_empty());
}

#[test]
fn all_invoke() {
    use mycroft_program::{Bar, Baz, Database};
    let mut db = Database::new();
    db.insert_bar(Bar { arg0: 3 });
    db.insert_bar(Bar { arg0: 42 });
    db.insert_bar(Bar { arg0: 3 });
    assert!(!db.rule_invoke_ordered_rule().is_empty());
    db.insert_baz(Baz {
        boom: 2,
        fizz: vec![3, 4],
        bash: 20,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 20,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 3,
    });
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![5, 46],
        bash: 42,
    });

    db.run_rules();

    assert_eq!(db.query_check_ordered().len(), 1);
    assert_eq!(db.query_check_named().len(), 2);
    assert_eq!(db.query_check_join().len(), 2);
}
