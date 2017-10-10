#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
use mycroft_macros::mycroft_program;

// This is here to be a non-copy type
type Vu16 = Vec<u16>;

mycroft_program!(
    r#"
bar(u64)
baz{boom: i32, fizz: Vu16}
"#
);

#[test]
fn insert_bar() {
    use predicates::bar::*;
    let mut bar = Storage::new();
    let (id_3, new_3) = bar.insert(Fact { arg0: 3 });
    let (id_42, new_42) = bar.insert(Fact { arg0: 42 });
    let (id_3_2, new_3_2) = bar.insert(Fact { arg0: 3 });
    assert_eq!(id_3, id_3_2);
    assert_ne!(id_3, id_42);
    assert!(new_3);
    assert!(new_42);
    assert!(!new_3_2);
}

#[test]
fn insert_baz() {
    use predicates::baz::*;
    let mut baz = Storage::new();
    let (id_3, new_3) = baz.insert(Fact {
        boom: 3,
        fizz: vec![72, 34],
    });
    let (id_3_off, new_3_off) = baz.insert(Fact {
        boom: 3,
        fizz: vec![],
    });
    let (id_3_2, new_3_2) = baz.insert(Fact {
        boom: 3,
        fizz: vec![72, 34],
    });
    assert_eq!(id_3, id_3_2);
    assert_ne!(id_3, id_3_off);
    assert!(new_3);
    assert!(new_3_off);
    assert!(!new_3_2);
}
