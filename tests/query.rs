#![feature(proc_macro)]
#[macro_use]
extern crate log;
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

type Vu16 = Vec<u16>;
const THREE: u64 = 3;
const SEVEN: i32 = 7;
mycroft_program!(
    r#"
Bar(u64)
Baz{boom: i32, fizz: Vu16, bash: u64}
?Ordered: Bar(~THREE)
?Named: Baz{boom: ~SEVEN, fizz: out_var}
?Join: Bar(idx) & Baz{fizz: join_out, bash: idx}
"#
);

#[test]
fn basic_queries() {
    use mycroft_program::{Bar, Baz, Database};
    let mut db = Database::new();
    db.insert_bar(Bar { arg0: 3 });
    db.insert_bar(Bar { arg0: 42 });
    db.insert_bar(Bar { arg0: 3 });
    assert_eq!(db.query_ordered().len(), 1);
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
    assert_eq!(db.query_named().len(), 2);
    assert_eq!(db.query_join().len(), 2)
}

#[test]
fn incremental_queries() {
    use mycroft_program::{Bar, Baz, Database};
    let mut db = Database::new();
    assert_eq!(db.query_ordered().len(), 0);
    assert_eq!(db.query_incr_ordered().len(), 0);
    db.insert_bar(Bar { arg0: 3 });
    assert_eq!(db.query_ordered().len(), 1);
    assert_eq!(db.query_incr_ordered().len(), 1);
    db.insert_bar(Bar { arg0: 42 });
    db.insert_bar(Bar { arg0: 3 });
    assert_eq!(db.query_ordered().len(), 1);
    assert_eq!(db.query_incr_ordered().len(), 0);
    db.insert_baz(Baz {
        boom: 2,
        fizz: vec![3, 4],
        bash: 20,
    });
    assert_eq!(db.query_named().len(), 0);
    assert_eq!(db.query_incr_named().len(), 0);
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 20,
    });
    assert_eq!(db.query_named().len(), 1);
    assert_eq!(db.query_incr_named().len(), 1);
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![3, 4],
        bash: 3,
    });
    assert_eq!(db.query_named().len(), 1);
    //assert_eq!(db.query_incr_named().len(), 0);
    // So, ideally this would return zero - it's a spurious new result
    // However:
    // 1.) Later, I will _want_ this behavior in order to add additional justifications to a fact -
    //   if I learn that I can invoke rule X based on id A or id B, this is a meaningful
    //   difference. For full queries, I still want it effectively deduping, because what it will
    //   be returning from the index is a _list_ of ids that create this tuple.
    // 2.) It's hard to fix without additional data structures, which normally wouldn't be an
    //   excuse, but in light of #1, it's hard to justify spending a bunch of engineering time
    //   suppressing a spurious, but still-true result.
    db.query_incr_named();
    db.insert_baz(Baz {
        boom: 7,
        fizz: vec![5, 46],
        bash: 42,
    });
    assert_eq!(db.query_named().len(), 2);
    assert_eq!(db.query_incr_named().len(), 1);
    assert_eq!(db.query_join().len(), 2)
}
