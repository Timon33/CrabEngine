use crate::{Board, Move};

pub struct Engine {
    pub current_board: Board,
    board_history: Vec<Board>,
    move_history: Vec<Move>,

    pub show_debug_information: bool,
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
            board.moves(&mut |b: &mut Board, _| {
                Self::perft_rec_callback(b, depth - 1, counter);
            });
        } else {
            *counter += 1;
        }
    }

    pub fn perft_test(&mut self, depth: u64) -> Vec<(Move, u64)> {
        let mut result: Vec<(Move, u64)> = Vec::new();
        let current_board_clone = self.current_board.clone();
        self.current_board.moves(&mut |current_board: &mut Board, uci_move: Move| {
            let mut count: u64 = 0;
            Self::perft_rec_callback(current_board, depth, &mut count);
            result.push((uci_move, count));
        });
        result
    }
}