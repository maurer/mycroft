#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

const THREE: u32 = 3;

mycroft_program!(
    r#"
Foo(String)
Bar{a: u32, b: u64}
?check_foo: Foo(out)
xlat_foos: Foo(out) <- Bar{a: ~THREE, b: x} + xlat
"#
);

use mycroft_program::*;

pub fn xlat<'a>(i: XlatIn<'a>) -> Vec<XlatOut> {
    let ss = vec![format!("{}", i.x), format!("{}.0", i.x)];
    ss.into_iter().map(|s| XlatOut { out: s }).collect()
}

#[test]
fn trivial() {
    let mut db = Database::new();
    db.insert_bar(Bar { a: 7, b: 4 });
    db.insert_bar(Bar { a: 3, b: 42 });
    db.run_rules();
    assert_eq!(
        vec!["42".to_string(), "42.0".to_string()],
        db.query_check_foo()
            .into_iter()
            .map(|x| x.out)
            .collect::<Vec<_>>()
    );
}
