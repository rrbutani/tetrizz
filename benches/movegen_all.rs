use criterion::{criterion_group, criterion_main, Criterion};
use tetrizz::data::*;
use tetrizz::movegen::baseline::movegen_piece;

pub fn criterion_benchmark(c: &mut Criterion) {
    let game = Game::new(None);
    c.bench_function("movegen all", |b| {
        b.iter(|| {
            {
                let p = Piece::I;
                movegen_piece(&game.board, p);
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
