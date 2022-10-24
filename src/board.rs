use crate::castling::CastlingRights;
use crate::piece::{ColorPiece, Piece};
use crate::player::Player;
use crate::{bitboard, castling, file, piece, player, square, BitBoard, Move, Square};
use std::char::from_digit;
use std::fmt::{Debug, Display, Formatter};

pub struct Board {
    // addressed using constants defined in piece.rs
    pub pieces: [BitBoard; 16],
    pub castling_rights: CastlingRights,
    side_to_play: Player,
    half_moves_since_capture: u32,
    pub half_moves: u32,
}

impl Board {
    /// completely empty board (invalid position)
    pub const fn empty_board() -> Board {
        Board {
            pieces: [bitboard::EMPTY; 16],
            castling_rights: castling::ALL_RIGHTS,
            side_to_play: player::WHITE,
            half_moves_since_capture: 0,
            half_moves: 0,
        }
    }

    /// sets up a board in the starting position
    pub fn starting_pos() -> Board {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    }

    /// load position for fen string
    pub fn from_fen(fen: &str) -> Board {
        let mut fen_parts = fen.split_whitespace();
        // 1. part of fen string notation piece position
        let placement_data: Vec<&str> = match fen_parts.next() {
            Some(s) => s.split("/").collect(),
            None => panic!(),
        };
        // build piece bitboards from first fen part
        let mut pieces: [BitBoard; 16] = [bitboard::EMPTY; 16];
        let mut current_sq: Square = square::A1;
        // reverse iterate the rank placement data (starting at 1st file)
        for rank_data in placement_data.into_iter().rev() {
            for c in rank_data.chars() {
                match piece::from_fen_char(c) {
                    Some(index) => {
                        pieces[index] |= bitboard::from_square(current_sq);
                        current_sq += 1;
                    }
                    None => {
                        if c.is_digit(10) {
                            current_sq += c.to_digit(10).unwrap() as u8; // save unwrap as c must be a digit
                        }
                    }
                }
            }
        }

        // 2. part is the active color
        let side_to_play = match fen_parts.next() {
            Some("w") => player::WHITE,
            Some("b") => player::BLACK,
            _ => panic!(),
        };

        // 3. castling rights
        let castling_rights = castling::from_fen(fen_parts.next().unwrap());

        // 4. en passant square
        pieces[piece::EN_PASSANT] = match fen_parts.next() {
            Some("-") => bitboard::EMPTY,
            Some(sq_str) => bitboard::from_square(square::from_string(sq_str)),
            None => panic!(),
        };

        // 5. half move clock
        let half_moves_since_capture: u32 = fen_parts.next().unwrap_or("0").parse().unwrap_or(0);

        // 6. full move clock
        let full_moves: u32 = fen_parts.next().unwrap_or("0").parse().unwrap_or(0);

        let mut result = Board {
            pieces,
            castling_rights,
            side_to_play,
            half_moves_since_capture,
            half_moves: full_moves / 2,
        };
        result.compute_occupations();
        result
    }

    // serialization

    /// current position in fen notation
    pub fn to_fen(&self) -> String {
        let mut result = String::new();
        let mut sq = square::A1;

        let mut rank_str = String::new();
        let mut free_squares: u32 = 0;

        while sq < 64 {
            let mut piece_found = false;
            for p in piece::ALL_PIECES {
                if self.get(p) & bitboard::from_square(sq) != bitboard::EMPTY {
                    piece_found = true;
                    if free_squares > 0 {
                        rank_str.push(from_digit(free_squares, 10).unwrap());
                        free_squares = 0;
                    }
                    rank_str.push(piece::to_char(p));
                    break;
                }
            }
            if !piece_found {
                free_squares += 1;
            }

            if file::from_square(sq) == file::FILE_H {
                if free_squares > 0 {
                    rank_str.push(from_digit(free_squares, 10).unwrap());
                    free_squares = 0;
                }
                if !result.is_empty() {
                    result = rank_str.clone() + "/" + &*result;
                } else {
                    result = rank_str.clone();
                }
                rank_str.clear();
            }
            sq += 1;
        }
        result.push_str(if self.white_to_play() { " w " } else { " b " });
        result.push_str(&*(self.castling_rights_fen() + " "));
        result.push_str(
            &*(square::to_string(bitboard::to_square(self.get(piece::EN_PASSANT))) + " "),
        );
        result.push_str(&*(self.half_moves_since_capture.to_string() + " "));
        result.push_str(&*self.full_moves().to_string());

        result
    }

    /// board as ascii printable board
    pub fn to_string(&self) -> String {
        let mut s = String::new();
        for r in (0..8).rev() {
            for f in 0..8 {
                let sq = bitboard::from_square(square::from_file_rank(f, r << 3));
                let mut piece_char = '-';
                for piece in piece::ALL_PIECES {
                    if sq & self.get(piece) != bitboard::EMPTY {
                        piece_char = piece::to_char(piece);
                        break;
                    }
                }

                s.push(piece_char);
                s.push(' ');
            }
            s.push_str("\n");
        }
        s
    }

    /// detailed board information
    pub fn debug(&self) -> String {
        format!(
            "---DEBUG---\n{}\nOccupations:\n{}\n{}\nAll:\n{}\nFen: {}",
            self.to_string(),
            bitboard::to_string(self.get(piece::WHITE_OCC)),
            bitboard::to_string(self.get(piece::BLACK_OCC)),
            bitboard::to_string(self.get(piece::ALL)),
            self.to_fen()
        )
    }

    /// compute occupation for both sides and all pieces together
    #[inline]
    fn compute_occupations(&mut self) {
        for piece in piece::WHITE_PIECES {
            self.pieces[piece::WHITE_OCC] |= self.pieces[piece];
        }
        for piece in piece::BLACK_PIECES {
            self.pieces[piece::BLACK_OCC] |= self.pieces[piece];
        }
        self.pieces[piece::ALL] = self.pieces[piece::BLACK_OCC] | self.pieces[piece::WHITE_OCC];
    }
    #[inline]
    pub fn copy_enemies_pieces<const IS_WHITE: bool>(&self) -> [BitBoard; 7] {
        let mut result: [BitBoard; 7] = [bitboard::EMPTY; 7];
        if IS_WHITE {
            result.copy_from_slice(&self.pieces[piece::BLACK_OCC..(piece::BLACK_KING + 1)]);
        } else {
            result.copy_from_slice(&self.pieces[piece::WHITE_OCC..(piece::WHITE_KING + 1)]);
        }
        result
    }
    #[inline]
    pub fn set_all_enemies_pieces<const IS_WHITE: bool>(&mut self, enemy_pieces: &[BitBoard; 7]) {
        if IS_WHITE {
            self.pieces[piece::BLACK_OCC..(piece::BLACK_KING + 1)].copy_from_slice(enemy_pieces);
        } else {
            self.pieces[piece::WHITE_OCC..(piece::WHITE_KING + 1)].copy_from_slice(enemy_pieces);
        }
    }
    #[inline]
    pub fn clear_enemy_from_square<const IS_WHITE: bool>(&mut self, capture: BitBoard) {
        if IS_WHITE {
            for piece in piece::BLACK_PIECES {
                self.pieces[piece] &= !capture;
            }
            self.pieces[piece::BLACK_OCC] &= !capture;
        } else {
            for piece in piece::WHITE_PIECES {
                self.pieces[piece] &= !capture;
            }
            self.pieces[piece::WHITE_OCC] &= !capture;
        }
    }

    #[inline]
    pub fn switch_player(&mut self) {
        self.side_to_play = player::switch_player(self.side_to_play);
    }
    #[inline]
    pub fn white_to_play(&self) -> bool {
        self.side_to_play == player::WHITE
    }
    #[inline]
    pub const fn full_moves(&self) -> u32 {
        self.half_moves / 2
    }

    #[inline]
    pub fn get(&self, piece: ColorPiece) -> BitBoard {
        self.pieces[piece]
    }
    #[inline]
    pub fn set(&mut self, piece: ColorPiece, val: BitBoard) {
        self.pieces[piece] = val;
    }

    // set and get relative to current side
    #[inline]
    pub const fn get_friendly<const IS_WHITE: bool, const PIECE: Piece>(&self) -> BitBoard {
        self.pieces[if IS_WHITE {
            piece::to_white(PIECE)
        } else {
            piece::to_black(PIECE)
        }]
    }
    #[inline]
    pub const fn get_enemy<const IS_WHITE: bool, const PIECE: Piece>(&self) -> BitBoard {
        self.pieces[if IS_WHITE {
            piece::to_black(PIECE)
        } else {
            piece::to_white(PIECE)
        }]
    }
    #[inline]
    pub fn set_friendly<const IS_WHITE: bool, const PIECE: Piece>(&mut self, val: BitBoard) {
        self.pieces[if IS_WHITE {
            piece::to_white(PIECE)
        } else {
            piece::to_black(PIECE)
        }] = val;
    }
    #[inline]
    pub fn set_enemy<const IS_WHITE: bool, const PIECE: Piece>(&mut self, val: BitBoard) {
        self.pieces[if IS_WHITE {
            piece::to_black(PIECE)
        } else {
            piece::to_white(PIECE)
        }] = val;
    }

    /// plays the given move on the board
    pub fn play_move(&mut self, m: Move) {
        // for all moves switch the moving piece
        if m.promotion.is_some() {
            self.pieces[m.piece] ^= m.from;
            self.pieces[m.promotion_piece.unwrap()] ^= m.promotion.unwrap();
        } else {
            self.pieces[m.piece] ^= m.from | m.to;
        }
        self.pieces[piece::OCC | piece::player_of(m.piece) as Piece] ^= m.from | m.to;

        // if there was a capture clear the captured piece
        if m.capture.is_some() {
            if self.white_to_play() {
                self.clear_enemy_from_square::<true>(m.capture.unwrap());
            } else {
                self.clear_enemy_from_square::<false>(m.capture.unwrap());
            }
        }

        self.pieces[piece::ALL] ^= m.from | m.to | m.capture.unwrap_or(bitboard::EMPTY);

        // check if the move was a castle and move the rook if so
        if piece::piece_type(m.piece) == piece::KING {
            let change = m.from | m.to;
            let rook_move = match change {
                castling::WKS_KING_CHANGE => castling::WKS_ROOK_CHANGE,
                castling::WQS_KING_CHANGE => castling::WQS_ROOK_CHANGE,
                castling::BKS_KING_CHANGE => castling::BKS_ROOK_CHANGE,
                castling::BQS_KING_CHANGE => castling::BQS_ROOK_CHANGE,
                _ => bitboard::EMPTY,
            };

            self.pieces[piece::ROOK | piece::player_of(m.piece) as Piece] ^= rook_move;
            self.pieces[piece::OCC | piece::player_of(m.piece) as Piece] ^= rook_move;
            self.pieces[piece::ALL] ^= rook_move;
        }
    }

    /// applies a silent (no capturing move)
    /// silent moves can be reverted by playing the "same" move again
    #[inline]
    pub fn apply_silent_move<const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        change: BitBoard,
    ) {
        self.set_friendly::<IS_WHITE, PIECE>(self.get_friendly::<IS_WHITE, PIECE>() ^ change);
        self.set_friendly::<IS_WHITE, { piece::OCC }>(
            self.get_friendly::<IS_WHITE, { piece::OCC }>() ^ change,
        );

        self.pieces[piece::ALL] ^= change;
    }

    #[inline]
    fn capture_helper<const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        from: BitBoard,
        to: BitBoard,
    ) {
        let change = from | to;
        self.set_friendly::<IS_WHITE, PIECE>(self.get_friendly::<IS_WHITE, PIECE>() ^ change);
        self.set_friendly::<IS_WHITE, { piece::OCC }>(
            self.get_friendly::<IS_WHITE, { piece::OCC }>() ^ change,
        );
        self.pieces[piece::ALL] ^= from;
    }

    /// applies a capture move, this deletes information about with enemy piece was captured
    /// so they have to be saved and restored later to undo
    /// NOTE: not used for en passant moves
    #[inline]
    pub fn apply_capture<const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        from: BitBoard,
        to: BitBoard,
    ) {
        self.capture_helper::<IS_WHITE, PIECE>(from, to);
        self.clear_enemy_from_square::<IS_WHITE>(to);
    }

    /// undo a capture, a old copy of the enemy pieces is required to restore them
    #[inline]
    pub fn undo_capture<const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        from: BitBoard,
        to: BitBoard,
        enemies: &[BitBoard; 7],
    ) {
        self.capture_helper::<IS_WHITE, PIECE>(from, to);
        self.set_all_enemies_pieces::<IS_WHITE>(enemies);
    }

    /// removes/sets a pawn for the given side, combined with silent move for en passant
    #[inline]
    pub fn en_passant_update<const IS_WHITE: bool>(&mut self, en_passant: BitBoard) {
        if IS_WHITE {
            self.pieces[piece::BLACK_PAWN] ^= en_passant;
            self.pieces[piece::BLACK_OCC] ^= en_passant;
            self.pieces[piece::ALL] ^= en_passant;
        } else {
            self.pieces[piece::WHITE_PAWN] ^= en_passant;
            self.pieces[piece::WHITE_OCC] ^= en_passant;
            self.pieces[piece::ALL] ^= en_passant;
        }
    }
}

impl Clone for Board {
    fn clone(&self) -> Self {
        Board {
            pieces: self.pieces.clone(),
            castling_rights: self.castling_rights.clone(),
            side_to_play: self.side_to_play.clone(),
            half_moves_since_capture: self.half_moves_since_capture.clone(),
            half_moves: self.half_moves.clone(),
        }
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl PartialEq for Board {
    fn eq(&self, other: &Self) -> bool {
        self.pieces
            .iter()
            .zip(other.pieces.iter())
            .all(|(x, y)| x == y) // check if all pieces bitboards are equal
            && self.side_to_play == other.side_to_play
            && self.castling_rights == other.castling_rights
            && self.half_moves == other.half_moves
            && self.half_moves_since_capture == other.half_moves_since_capture
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug())
    }
}
