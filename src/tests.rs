use crate::{Board, Engine};
use std::iter::zip;
use std::time;

pub fn measure_move_gen_performance(board: Board, search_depth: u64) -> (u64, time::Duration) {
    let mut engine = Engine::initialize(board);

    let start_time = time::Instant::now();
    let move_count = engine.perft(search_depth);
    let duration = start_time.elapsed();

    (move_count, duration)
}

pub fn run_performance_test() {
    println!("Starting performance tests.\nCalculating perft 6 from startpos");
    let test_board = Board::starting_pos();
    let (move_count, duration) = measure_move_gen_performance(test_board, 6);
    println!(
        "Calculated {} moves in {} ms",
        move_count,
        duration.as_nanos() as f64 / 1e6
    );
    println!(
        "Moves per s: {}",
        move_count as f64 / duration.as_nanos() as f64 * 1e9
    );
}

pub fn run_perft_test() {
    println!("Starting perft test to check move generation for correctness");

    // position and results from https://www.chessprogramming.org/Perft_Results
    let positions: [Board; 6] = [
        Board::starting_pos(),
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "),
        Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - "),
        Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"),
        Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  "),
        Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ",
        ),
    ];

    let expected_results: [[u64; 5]; 6] = [
        [20, 400, 8902, 197281, 4865609],
        [48, 2039, 97862, 4085603, 193690690],
        [14, 191, 2812, 43238, 674624],
        [6, 264, 9467, 422333, 15833292],
        [44, 1486, 62379, 2103487, 89941194],
        [46, 2079, 89890, 3894594, 164075551],
    ];

    for (pos, res) in zip(positions, expected_results) {
        for i in 0..5 {
            assert_eq!(
                res[i],
                measure_move_gen_performance(pos.clone(), (i + 1) as u64).0,
                "Error in perft {} from position {}.\n",
                i + 1,
                pos.to_fen()
            );
        }
    }

    println!("All perft test correct!");
}
