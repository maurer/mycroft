#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

type Vu16 = Vec<u16>;

fn u8_plus(x: u8, y: u8) -> u8 {
    x + y
}

fn vu16_append(x: &Vu16, y: &Vu16) -> Vu16 {
    let mut out = x.clone();
    out.extend(y);
    out.sort();
    out
}

mycroft_program!(
    r#"
Foo(u8^u8_plus)
Bar(Vu16^vu16_append)
?get_foo: Foo(out)
?get_bar: Bar(out)
"#
);

#[test]
fn insert_aggregate_u8() {
    use mycroft_program::{Database, Foo, GetFooResult};
    let mut db = Database::new();
    assert_eq!(db.query_get_foo(), vec![]);
    db.insert_foo(Foo { arg0: 3 });
    db.insert_foo(Foo { arg0: 42 });
    assert_eq!(db.query_get_foo(), vec![GetFooResult { out: 45 }]);
}

#[test]
fn insert_aggregate_vu16() {
    use mycroft_program::{Bar, Database, GetBarResult};
    let mut db = Database::new();
    assert_eq!(db.query_get_bar(), vec![]);
    db.insert_bar(Bar {
        arg0: vec![1, 3, 5],
    });
    db.insert_bar(Bar {
        arg0: vec![2, 4, 8],
    });
    assert_eq!(
        db.query_get_bar(),
        vec![
            GetBarResult {
                out: vec![1, 2, 3, 4, 5, 8],
            },
        ]
    );
}
