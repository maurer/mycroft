#![feature(proc_macro)]
#[macro_use]
extern crate log;
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

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
Bar(u8)
?get_bar: Bar(out)
advance_three: Bar(~(0)) <- ~Foo(~(3))
advance_ten: Bar(~(1)) <- ~Foo(~(10))
"#
);

#[test]
fn circ_invalidate() {
    use mycroft_program::{Database, Foo, GetBarResult};
    let mut db = Database::new();
    assert_eq!(db.query_get_bar(), vec![]);
    db.insert_foo(Foo { arg0: 3 });
    db.run_rules();
    assert_eq!(db.query_get_bar(), vec![GetBarResult { out: 0 }]);
    db.insert_foo(Foo { arg0: 7 });
    db.run_rules();
    assert_eq!(db.query_get_bar(), vec![GetBarResult { out: 1 }]);
}
