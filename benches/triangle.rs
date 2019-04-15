#![feature(test)]
extern crate test;

use mycroft_macros::mycroft_program;
use test::Bencher;

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
    b.iter(|| test::black_box(db.query_triangle()))
}

#[bench]
fn triangle_1k(b: &mut Bencher) {
    triangle_n(1_000, b)
}

#[bench]
fn triangle_10k(b: &mut Bencher) {
    triangle_n(10_000, b)
}
