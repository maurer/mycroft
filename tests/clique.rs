#![feature(proc_macro)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;

use mycroft_macros::mycroft_program;

const ZERO: u64 = 0;
const END: u64 = 10;
mycroft_program!(
    r#"
reachable(u64, u64)
edge(u64, u64)
same_clique(u64, u64)
?clique: same_clique(~ZERO, ~END)
promote: reachable(x, y) <- edge(x, y)
extend: reachable(x, z) <- edge(x, y) & reachable(y, z)
clique: same_clique(x, y) <- reachable(x, y) & reachable(y, x)
"#
);

#[test]
fn clique() {
    use mycroft_program::{Database, Edge};
    let mut db = Database::new();
    for i in ZERO..END {
        db.insert_edge(Edge {
            arg0: i,
            arg1: i + 1,
        });
    }
    db.insert_edge(Edge {
        arg0: END,
        arg1: ZERO,
    });
    db.run_rules();
    assert_eq!(db.query_clique().len(), 1);
}
