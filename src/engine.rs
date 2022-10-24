use crate::{Board, Move};

pub struct Engine {
    pub current_board: Board,
    board_history: Vec<Board>,
    move_history: Vec<Move>,

    pub show_debug_information: bool
}

impl Engine {
    pub fn initialize(board: Board) -> Self {
        Engine {
            current_board: board,
            board_history: vec![],
            move_history: vec![],
            show_debug_information: false,
        }
    }

    fn perft_rec_callback(board: &mut Board, depth: u64, counter: &mut u64) {
        if depth > 1 {
            board.generate_legal_moves(&mut |b: &mut Board, _| {
                Self::perft_rec_callback(b, depth - 1, counter);
            });
        } else {
            *counter += 1;
        }
    }

    pub fn dive_test(&mut self, depth: u64) -> Vec<(Move, u64)> {
        let mut result: Vec<(Move, u64)> = Vec::new();
        self.current_board.generate_legal_moves(&mut |b: &mut Board, uci_move: Move| {
            let mut count: u64 = 0;
            Self::perft_rec_callback(b, depth, &mut count);
            result.push((uci_move, count));
        });
        result
    }

    pub fn perft(&mut self, depth: u64) -> u64 {
        let mut count: u64 = 0;
        self.current_board.generate_legal_moves(&mut |b: &mut Board, _| {
            Self::perft_rec_callback(b, depth, &mut count);
        });
        count
    }

    pub fn ponder(&mut self) {

    }

    pub fn stop(&mut self) {

    }
}