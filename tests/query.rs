#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

type Vu16 = Vec<u16>;
const THREE: u64 = 3;
const SEVEN: i32 = 7;
mycroft_program!(r#"
Bar(u64)
Baz{boom: i32, fizz: Vu16, bash: u64}
?Ordered: Bar(~THREE)
?Named: Baz{boom: ~SEVEN, fizz: out_var}
?Join: Bar(idx) & Baz{fizz: join_out, bash: idx}
"#);

#[test]
fn basic_queries() {
    use mycroft_program::{Database, Bar, Baz};
    let mut db = Database::new();
    db.insert_bar(Bar { arg0: 3 });
    db.insert_bar(Bar { arg0: 42 });
    db.insert_bar(Bar { arg0: 3 });
    assert_eq!(db.query_ordered().len(), 1);
    db.insert_baz(Baz {boom: 2, fizz: vec![3, 4], bash: 20});
    db.insert_baz(Baz {boom: 7, fizz: vec![3, 4], bash: 20});
    db.insert_baz(Baz {boom: 7, fizz: vec![3, 4], bash: 3});
    db.insert_baz(Baz {boom: 7, fizz: vec![5, 46], bash: 42});
    assert_eq!(db.query_named().len(), 2);
    assert_eq!(db.query_join().len(), 2)
}
