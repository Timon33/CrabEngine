use crate::player::Player;

const PIECE_CHARS: [char; 16] = [
    '-', 'P', 'R', 'N', 'B', 'Q', 'K', '-', '-', 'p', 'r', 'n', 'b', 'q', 'k', '-',
];

// piece referring to either the white or black version
pub type Piece = usize;

pub const ALL: Piece = 7;
pub const EN_PASSANT: Piece = 15;

pub const OCC: Piece = 0;
pub const PAWN: Piece = 1;
pub const ROOK: Piece = 2;
pub const KNIGHT: Piece = 3;
pub const BISHOP: Piece = 4;
pub const QUEEN: Piece = 5;
pub const KING: Piece = 6;

// piece with given color
pub type ColorPiece = usize;

pub const WHITE_OCC: ColorPiece = 0;
pub const WHITE_PAWN: ColorPiece = 1;
pub const WHITE_ROOK: ColorPiece = 2;
pub const WHITE_KNIGHT: ColorPiece = 3;
pub const WHITE_BISHOP: ColorPiece = 4;
pub const WHITE_QUEEN: ColorPiece = 5;
pub const WHITE_KING: ColorPiece = 6;

pub const BLACK_OCC: ColorPiece = 8;
pub const BLACK_PAWN: ColorPiece = 9;
pub const BLACK_ROOK: ColorPiece = 10;
pub const BLACK_KNIGHT: ColorPiece = 11;
pub const BLACK_BISHOP: ColorPiece = 12;
pub const BLACK_QUEEN: ColorPiece = 13;
pub const BLACK_KING: ColorPiece = 14;

pub const WHITE_PIECES: [ColorPiece; 6] = [WHITE_PAWN, WHITE_ROOK, WHITE_KNIGHT, WHITE_BISHOP, WHITE_QUEEN, WHITE_KING];
pub const BLACK_PIECES: [ColorPiece; 6] = [BLACK_PAWN, BLACK_ROOK, BLACK_KNIGHT, BLACK_BISHOP, BLACK_QUEEN, BLACK_KING];

pub const ALL_PIECES: [ColorPiece; 12] = [WHITE_PAWN, WHITE_ROOK, WHITE_KNIGHT, WHITE_BISHOP, WHITE_QUEEN, WHITE_KING,
    BLACK_PAWN, BLACK_ROOK, BLACK_KNIGHT, BLACK_BISHOP, BLACK_QUEEN, BLACK_KING];

#[inline]
pub const fn swap_color(piece: ColorPiece) -> ColorPiece {
    piece ^ 8
}
#[inline]
pub const fn to_white(piece: Piece) -> ColorPiece {
    piece
}
#[inline]
pub const fn to_black(piece: Piece) -> ColorPiece {
    piece | 8
}
#[inline]
pub const fn player_of(piece: ColorPiece) -> Player {
    (piece & 8) as Player
}
#[inline]
pub const fn piece_type(piece: ColorPiece) -> Piece {
    piece & 7
}
#[inline]
pub const fn to_char(piece: ColorPiece) -> char {
    PIECE_CHARS[piece]
}
#[inline]
pub fn from_fen_char(fen_char: char) -> Option<ColorPiece> {
    PIECE_CHARS.iter().position(|&x| x == fen_char)
}