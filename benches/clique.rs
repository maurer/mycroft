#![feature(proc_macro, test)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;
extern crate test;

use mycroft_macros::mycroft_program;
use test::Bencher;

mycroft_program!(
    r#"
reachable(u64, u64)
edge(u64, u64)
same_clique(u64, u64)
promote: reachable(x, y) <- edge(x, y)
extend: reachable(x, z) <- edge(x, y) & reachable(y, z)
clique: same_clique(x, y) <- reachable(x, y) & reachable(y, x)
"#
);

fn clique_n(n: u64, b: &mut Bencher) {
    use mycroft_program::{Database, Edge};
    b.iter(|| {
        let mut db = Database::new();
        for i in 0..n {
            db.insert_edge(Edge {
                arg0: i,
                arg1: i + 1,
            });
        }
        db.insert_edge(Edge { arg0: n, arg1: 0 });
        db.run_rules();
        test::black_box(db)
    })
}

#[bench]
fn clique_10(b: &mut Bencher) {
    clique_n(10, b)
}

#[bench]
fn clique_20(b: &mut Bencher) {
    clique_n(20, b)
}

#[bench]
fn clique_30(b: &mut Bencher) {
    clique_n(30, b)
}

#[bench]
fn clique_40(b: &mut Bencher) {
    clique_n(40, b)
}

#[bench]
fn clique_50(b: &mut Bencher) {
    clique_n(50, b)
}

#[bench]
fn clique_60(b: &mut Bencher) {
    clique_n(60, b)
}

#[bench]
fn clique_70(b: &mut Bencher) {
    clique_n(70, b)
}

#[bench]
fn clique_80(b: &mut Bencher) {
    clique_n(80, b)
}

#[bench]
fn clique_90(b: &mut Bencher) {
    clique_n(90, b)
}

#[bench]
fn clique_100(b: &mut Bencher) {
    clique_n(100, b)
}
