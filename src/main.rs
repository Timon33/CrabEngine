#![feature(const_eval_limit)]
#![const_eval_limit = "0"]
#![feature(str_split_whitespace_as_str)]

extern crate core;

use crate::bitboard::BitBoard;
use crate::board::Board;
use crate::engine::Engine;
use crate::file::File;
use crate::moves::Move;
use crate::rank::Rank;
use crate::square::Square;

mod square;
mod file;
mod rank;
mod bitboard;
mod luts;
mod board;
mod piece;
mod move_gen;
mod castling;
mod player;
mod moves;
mod uci;
mod engine;

fn asserter(board: &mut Board, depth: u64) {
    if depth > 0 {
        let clone = board.clone();
        board.moves(&mut |b: &mut Board, _| asserter(b, depth - 1));
        assert_eq!(&clone, board, "eq failed\n{}\n{}", clone, board);
    }
}


fn show_moves(board: &mut Board, m: Move) {
    println!("{}{}\n", board, m);
}

fn move_debug<T: FnMut(&mut Board, Move)>(board: &mut Board, callback: &mut T) {
    let check_mask = board.check_mask::<true>(board.pieces[piece::EN_PASSANT]);
    if board.white_to_play() {
        board.king_moves::<T, true>(callback); //, board.pieces[piece::EN_PASSANT], check_mask);
    } else {
        board.king_moves::<T, false>(callback); // , board.pieces[piece::EN_PASSANT], check_mask);
    }
}

fn main() {
    let mut board = Board::from_fen("r3k3/p1ppqpb1/bn2pnp1/3PN1B1/1p2PP2/2NQ4/PPP1BK1r/R5bR w - - 0 3");
    // println!("{}", board.double_check::<true>());
    // move_debug(&mut board, &mut show_moves);

    uci::processing_loop();
}

