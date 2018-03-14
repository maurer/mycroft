#[macro_use]
extern crate log;
extern crate mycroft;
#[macro_use]
extern crate mycroft_macros;
extern crate mycroft_support;

use std::time::Duration;

use mycroft_program::{IncIn, IncOut};

fn inc(i: &IncIn) -> Vec<IncOut> {
    vec![
        IncOut {
            n_plus_one: i.n + 1,
        },
    ]
}

mycroft_program!(
    r#"
Foo(u64)
inc_foo: Foo(n_plus_one) <- Foo(n) + inc
"#
);

#[test]
fn check_timeout() {
    use mycroft_program::{Database, Foo};
    let mut db = Database::new();
    db.insert_foo(Foo { arg0: 3 });
    db.run_rules_with_timeout(Some(Duration::new(1, 0)));
}
