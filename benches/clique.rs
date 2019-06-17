use criterion::{black_box, criterion_group, criterion_main, Bencher, Criterion};
use mycroft_macros::mycroft_program;

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
    use crate::mycroft_program::{Database, Edge};
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
        black_box(db)
    })
}

fn clique_benches(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "clique",
        |b, &&n| clique_n(n, b),
        &[10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    );
}

criterion_group!(benches, clique_benches);
criterion_main!(benches);
