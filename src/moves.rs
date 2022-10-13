use crate::piece::{ColorPiece, Piece};
use crate::{bitboard, piece, player, square, BitBoard, Board};
use std::fmt::{Display, Formatter};

pub struct Move {
    pub piece: ColorPiece,
    pub from: BitBoard,
    pub to: BitBoard,
    pub capture: Option<BitBoard>,
    pub promotion_piece: Option<Piece>,
    pub promotion: Option<BitBoard>,
}

impl Move {
    pub fn to_uci_move(&self) -> String {
        let mut s = square::to_string(bitboard::to_square(self.from))
            + &*square::to_string(bitboard::to_square(self.to));
        if self.promotion.is_some() {
            s.push(
                piece::to_char(self.promotion_piece.unwrap())
                    .to_lowercase()
                    .next()
                    .unwrap(),
            );
        }
        s
    }

    pub fn new_silent(piece: Piece, from: BitBoard, to: BitBoard) -> Move {
        Move {
            piece,
            from,
            to,
            capture: None,
            promotion_piece: None,
            promotion: None,
        }
    }

    pub fn new_capture(piece: Piece, from: BitBoard, to: BitBoard, capture: BitBoard) -> Move {
        Move {
            piece,
            from,
            to,
            capture: Some(capture),
            promotion_piece: None,
            promotion: None,
        }
    }

    pub fn new_promotion(
        piece: Piece,
        from: BitBoard,
        to: BitBoard,
        promotion_piece: Piece,
        promotion: BitBoard,
    ) -> Move {
        Move {
            piece,
            from,
            to,
            capture: None,
            promotion_piece: Some(promotion_piece),
            promotion: Some(promotion),
        }
    }
}

impl Clone for Move {
    fn clone(&self) -> Self {
        Move {
            piece: self.piece,
            from: self.from,
            to: self.to,
            capture: self.capture.clone(),
            promotion_piece: self.promotion_piece.clone(),
            promotion: self.promotion.clone(),
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_uci_move())
    }
}
