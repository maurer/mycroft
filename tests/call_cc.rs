use mycroft_macros::mycroft_program;

fn u8_plus(xs: &[u8]) -> u8 {
    let mut out = 0;
    for x in xs {
        out += *x;
    }
    out
}

mycroft_program!(
    r#"
Foo(u8^u8_plus)
Bar(u8^u8_plus)
Baz(u8^u8_plus)
?get_foo: Foo(out)
?get_baz: Baz(out)
ohno: Foo(~(3)) <- ~Foo(~(0))
retract_init: Baz(~(0)) <- ~Bar(~(0))
baz_add: Baz(~(4)) <- ~Baz(~(0))
"#
);

// The core problem here is that when retract_init triggers, it succesfully retracts Baz(~(0)),
// but Baz(~(4)) is still considered "justified" by *itself*, since the meta-ids were swapped
// during call/cc
// Basically, the meta-id for "Baz is 0" got swapped for the one that says "Baz is 4", and when
// re-evaluating, it says "Baz sums to 4, we're still good"
// This test program *accidentally* hits the corner case of "removing Baz doesn't modify the sum"

#[test]
fn circ_cc() {
    use crate::mycroft_program::{Bar, Database, Foo, GetBazResult, GetFooResult};
    let mut db = Database::new();
    println!("Simple insert");
    db.insert_foo(Foo { arg0: 0 });
    db.run_rules();
    assert_eq!(db.query_get_foo(), vec![GetFooResult { out: 3 }]);
    println!("Triggering insert");
    db.insert_bar(Bar { arg0: 0 });
    db.run_rules();
    assert_eq!(db.query_get_baz(), vec![GetBazResult { out: 4 }]);
    println!("Triggering removal");
    db.insert_bar(Bar { arg0: 0 });
    println!("Running rules");
    db.run_rules();
    assert_eq!(db.query_get_foo(), vec![GetFooResult { out: 3 }]);
    println!("get_baz");
    assert_eq!(db.query_get_baz(), vec![]);
}
