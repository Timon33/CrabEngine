use crate::{BitBoard, bitboard, Board, Move, piece, square};

/// Castling right as bitmask
/// _QK _QK (lower 6 bits)
/// -b- -w-
pub type CastlingRights = u8;

pub const ALL_RIGHTS: CastlingRights = 0x1B;
pub const NO_RIGHTS: CastlingRights = 0;

pub const WHITE_KING_SIDE: CastlingRights = 0x1;
pub const WHITE_QUEEN_SIDE: CastlingRights = 0x2;
pub const BLACK_KING_SIDE: CastlingRights = 0x8;
pub const BLACK_QUEEN_SIDE: CastlingRights = 0x10;

pub const WHITE_KING_SIDE_MASK: BitBoard = 0x70;
pub const WHITE_QUEEN_SIDE_OCC_MASK: BitBoard = 0xE;
pub const WHITE_QUEEN_SIDE_DANGER_MASK: BitBoard = 0x1C;
pub const BLACK_KING_SIDE_MASK: BitBoard = 0x7000000000000000;
pub const BLACK_QUEEN_SIDE_OCC_MASK: BitBoard = 0xE00000000000000;
pub const BLACK_QUEEN_SIDE_DANGER_MASK: BitBoard = 0x1C00000000000000;

pub const WKS_KING_CHANGE: BitBoard = 0x50;
pub const WKS_ROOK_CHANGE: BitBoard = 0xA0;
pub const WQS_KING_CHANGE: BitBoard = 0x14;
pub const WQS_ROOK_CHANGE: BitBoard = 0x9;
pub const BKS_KING_CHANGE: BitBoard = 0x5000000000000000;
pub const BKS_ROOK_CHANGE: BitBoard = 0xA000000000000000;
pub const BQS_KING_CHANGE: BitBoard = 0x1400000000000000;
pub const BQS_ROOK_CHANGE: BitBoard = 0x0900000000000000;

pub fn from_fen(castling_fen: &str) -> CastlingRights {
    let mut rights = NO_RIGHTS;
    for c in castling_fen.chars() {
        rights |= match c {
            'K' => WHITE_KING_SIDE,
            'Q' => WHITE_QUEEN_SIDE,
            'k' => BLACK_KING_SIDE,
            'q' => BLACK_QUEEN_SIDE,
            _ => NO_RIGHTS
        }
    }
    rights
}

impl Board {

    pub fn castling_rights_fen(&self) -> String {
        let mut res = String::new();
        if self.king_side_rights::<true>() { res += "K" }
        if self.queen_side_rights::<true>() { res += "Q" }
        if self.king_side_rights::<false>() { res += "k" }
        if self.queen_side_rights::<false>() { res += "q" }
        if res.is_empty() { res = String::from("-") }
        res
    }

    #[inline]
    pub fn king_side_rights<const IS_WHITE: bool>(&self) -> bool {
        if IS_WHITE {
            WHITE_KING_SIDE & self.castling_rights != 0
        } else {
            BLACK_KING_SIDE & self.castling_rights != 0
        }
    }
    #[inline]
    pub fn queen_side_rights<const IS_WHITE: bool>(&self) -> bool {
        if IS_WHITE {
            WHITE_QUEEN_SIDE & self.castling_rights != 0
        } else {
            BLACK_QUEEN_SIDE & self.castling_rights != 0
        }
    }
    #[inline]
    pub fn clear_rights<const IS_WHITE: bool>(&mut self) {
        if IS_WHITE {
            self.castling_rights &= BLACK_KING_SIDE | BLACK_QUEEN_SIDE;
        } else {
            self.castling_rights &= WHITE_KING_SIDE | WHITE_QUEEN_SIDE;
        }
    }
    #[inline]
    pub fn king_castling_blocked<const IS_WHITE: bool>(&self, occ: BitBoard, danger: BitBoard) -> bool {
        if IS_WHITE {
            ((occ & !bitboard::from_square(square::E1)) | danger) & WHITE_KING_SIDE_MASK != bitboard::EMPTY
        } else {
            ((occ & !bitboard::from_square(square::E8)) | danger) & BLACK_KING_SIDE_MASK != bitboard::EMPTY
        }
    }
    #[inline]
    pub fn queen_castling_blocked<const IS_WHITE: bool>(&self, occ: BitBoard, danger: BitBoard) -> bool {
        if IS_WHITE {
            (occ & WHITE_QUEEN_SIDE_OCC_MASK) | (danger & WHITE_QUEEN_SIDE_DANGER_MASK) != bitboard::EMPTY
        } else {
            (occ & BLACK_QUEEN_SIDE_OCC_MASK) | (danger & BLACK_QUEEN_SIDE_DANGER_MASK) != bitboard::EMPTY
        }
    }
    #[inline]
    pub fn castle_king_side<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(&mut self, callback: &mut T) {
        let old_rights = self.castling_rights;
        self.clear_rights::<IS_WHITE>();
        if IS_WHITE {
            self.apply_silent_move::<IS_WHITE, { piece::KING }>(WKS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(WKS_ROOK_CHANGE);

            callback(self, Move::new_silent(piece::KING, bitboard::from_square(square::E1), bitboard::from_square(square::G1)));

            self.apply_silent_move::<IS_WHITE, { piece::KING }>(WKS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(WKS_ROOK_CHANGE);
        } else {
            self.apply_silent_move::<IS_WHITE, { piece::KING }>(BKS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(BKS_ROOK_CHANGE);

            callback(self, Move::new_silent(piece::KING, bitboard::from_square(square::E8), bitboard::from_square(square::G8)));

            self.apply_silent_move::<IS_WHITE, { piece::KING }>(BKS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(BKS_ROOK_CHANGE);
        }
        self.castling_rights = old_rights;
    }
    #[inline]
    pub fn castle_queen_side<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(&mut self, callback: &mut T) {
        let old_rights = self.castling_rights;
        self.clear_rights::<IS_WHITE>();
        if IS_WHITE {
            self.apply_silent_move::<IS_WHITE, { piece::KING }>(WQS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(WQS_ROOK_CHANGE);

            callback(self, Move::new_silent(piece::KING, bitboard::from_square(square::E1), bitboard::from_square(square::C1)));

            self.apply_silent_move::<IS_WHITE, { piece::KING }>(WQS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(WQS_ROOK_CHANGE);
        } else {
            self.apply_silent_move::<IS_WHITE, { piece::KING }>(BQS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(BQS_ROOK_CHANGE);

            callback(self, Move::new_silent(piece::KING, bitboard::from_square(square::E8), bitboard::from_square(square::C8)));

            self.apply_silent_move::<IS_WHITE, { piece::KING }>(BQS_KING_CHANGE);
            self.apply_silent_move::<IS_WHITE, { piece::ROOK }>(BQS_ROOK_CHANGE);
        }
        self.castling_rights = old_rights;
    }
}