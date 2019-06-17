use criterion::{black_box, criterion_group, criterion_main, Bencher, Criterion};
use mycroft_macros::mycroft_program;

mycroft_program!("P(usize)");

fn insert_n(n: usize, b: &mut Bencher) {
    use crate::mycroft_program::{Database, P};
    let mut rng = Vec::new();
    for _ in 0..n {
        rng.push(rand::random());
    }
    b.iter(|| {
        let mut p = Database::new();
        for n in &rng {
            p.insert_p(P { arg0: *n });
        }
        black_box(p)
    })
}

fn insert_benches(c: &mut Criterion) {
    c.bench_function_over_inputs("insert", |b, &&n| insert_n(n, b), &[1_000, 10_000, 100_000]);
}

criterion_group!(benches, insert_benches);
criterion_main!(benches);
