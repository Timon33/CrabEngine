#![feature(const_eval_limit)]
#![const_eval_limit = "0"]
#![feature(str_split_whitespace_as_str)]

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
mod tests;

fn main() {
    uci::processing_loop();
}

