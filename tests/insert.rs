#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

// This is here to be a non-copy type
type Vu16 = Vec<u16>;

mycroft_program!(
    r#"
Bar(u64)
Baz{boom: i32, fizz: Vu16}
"#
);

#[test]
fn insert_bar() {
    use mycroft_program::{Bar, Database};
    let mut db = Database::new();
    let id_3 = db.insert_bar(Bar { arg0: 3 });
    let id_42 = db.insert_bar(Bar { arg0: 42 });
    let id_3_2 = db.insert_bar(Bar { arg0: 3 });
    assert_eq!(id_3, id_3_2);
    assert_ne!(id_3, id_42);
}

#[test]
fn insert_baz() {
    use mycroft_program::{Baz, Database};
    let mut db = Database::new();
    let id_3 = db.insert_baz(Baz {
        boom: 3,
        fizz: vec![72, 34],
    });
    let id_3_off = db.insert_baz(Baz {
        boom: 3,
        fizz: vec![],
    });
    let id_3_2 = db.insert_baz(Baz {
        boom: 3,
        fizz: vec![72, 34],
    });
    assert_eq!(id_3, id_3_2);
    assert_ne!(id_3, id_3_off);
}
