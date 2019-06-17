use criterion::{black_box, criterion_group, criterion_main, Bencher, Criterion};
use mycroft_macros::mycroft_program;

mycroft_program!(
    "
P(usize, usize)
Q(usize, usize)
R(usize, usize)
?Triangle: P(x, y) & Q(y, z) & R(z, x)
"
);

fn triangle_n(n: usize, b: &mut Bencher) {
    use crate::mycroft_program::{Database, P, Q, R};
    let mut db = Database::new();
    for _ in 0..n {
        db.insert_p(P {
            arg0: rand::random(),
            arg1: rand::random(),
        });
        db.insert_q(Q {
            arg0: rand::random(),
            arg1: rand::random(),
        });
        db.insert_r(R {
            arg0: rand::random(),
            arg1: rand::random(),
        });
    }
    b.iter(|| black_box(db.query_triangle()))
}

fn triangle_benches(c: &mut Criterion) {
    c.bench_function_over_inputs("triangle", |b, &&n| triangle_n(n, b), &[1_000, 10_000]);
}

criterion_group!(benches, triangle_benches);
criterion_main!(benches);
