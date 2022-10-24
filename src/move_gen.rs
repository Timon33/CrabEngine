use crate::piece::Piece;
use crate::{bitboard, castling, luts, piece, rank, square, BitBoard, Board, Move, Square};

/// pawn attack mask for given side of pawns
#[inline]
pub fn pawn_attacks<const IS_WHITE: bool>(pawns: BitBoard) -> BitBoard {
    if IS_WHITE {
        (pawns << 7) & 0x7F7F7F7F7F7F7F7F | (pawns << 9) & 0xFEFEFEFEFEFEFEFE
    } else {
        (pawns >> 9) & 0x7F7F7F7F7F7F7F7F | (pawns >> 7) & 0xFEFEFEFEFEFEFEFE
    }
}

/// single forward pawn moves
#[inline]
pub fn pawn_moves<const IS_WHITE: bool>(pawns: BitBoard, occ: BitBoard) -> BitBoard {
    if IS_WHITE {
        pawns << 8 & !occ
    } else {
        pawns >> 8 & !occ
    }
}

/// double pawn moves from startposition of pawns
#[inline]
pub fn double_pawn_moves<const IS_WHITE: bool>(pawns: BitBoard, occ: BitBoard) -> BitBoard {
    if IS_WHITE {
        ((pawns & bitboard::rank_mask_of_sq(square::A2)) << 16) & !(occ | (occ << 8))
    } else {
        ((pawns & bitboard::rank_mask_of_sq(square::A7)) >> 16) & !(occ | (occ >> 8))
    }
}

impl Board {
    /// calls back for evey possible promotion of pawn
    #[inline]
    fn promote_piece<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        from: BitBoard, // old pawn position
        to: BitBoard, // position of promoted piece
    ) {
        self.set_friendly::<IS_WHITE, { piece::ROOK }>(
            self.get_friendly::<IS_WHITE, { piece::ROOK }>() ^ to,
        );
        let rook_move = Move::new_promotion(piece::PAWN, from, to, piece::ROOK, to);
        callback(self, rook_move);
        self.set_friendly::<IS_WHITE, { piece::ROOK }>(
            self.get_friendly::<IS_WHITE, { piece::ROOK }>() ^ to,
        );

        self.set_friendly::<IS_WHITE, { piece::KNIGHT }>(
            self.get_friendly::<IS_WHITE, { piece::KNIGHT }>() ^ to,
        );
        let knight_move = Move::new_promotion(piece::PAWN, from, to, piece::KNIGHT, to);
        callback(self, knight_move);
        self.set_friendly::<IS_WHITE, { piece::KNIGHT }>(
            self.get_friendly::<IS_WHITE, { piece::KNIGHT }>() ^ to,
        );

        self.set_friendly::<IS_WHITE, { piece::BISHOP }>(
            self.get_friendly::<IS_WHITE, { piece::BISHOP }>() ^ to,
        );
        let bishop_move = Move::new_promotion(piece::PAWN, from, to, piece::BISHOP, to);
        callback(self, bishop_move);
        self.set_friendly::<IS_WHITE, { piece::BISHOP }>(
            self.get_friendly::<IS_WHITE, { piece::BISHOP }>() ^ to,
        );

        self.set_friendly::<IS_WHITE, { piece::QUEEN }>(
            self.get_friendly::<IS_WHITE, { piece::QUEEN }>() ^ to,
        );
        let queen_move = Move::new_promotion(piece::PAWN, from, to, piece::QUEEN, to);
        callback(self, queen_move);
        self.set_friendly::<IS_WHITE, { piece::QUEEN }>(
            self.get_friendly::<IS_WHITE, { piece::QUEEN }>() ^ to,
        );
    }

    /// silent (no capture) moves to every square in moves
    #[inline]
    fn silent_moves_loop<T: FnMut(&mut Board, Move), const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        callback: &mut T,
        from: BitBoard,
        mut moves: BitBoard, // all possible destinations of piece at from
    ) {
        while moves != bitboard::EMPTY {
            let to = bitboard::extract_lsb(&mut moves);

            if PIECE == piece::PAWN {
                // check for promotions
                if to & (bitboard::rank_mask(rank::RANK_1) | bitboard::rank_mask(rank::RANK_8))
                    != bitboard::EMPTY
                {
                    self.apply_silent_move::<IS_WHITE, PIECE>(from);

                    self.pieces[piece::ALL] ^= to;
                    self.pieces[if IS_WHITE {
                        piece::WHITE_OCC
                    } else {
                        piece::BLACK_OCC
                    }] ^= to;
                    self.promote_piece::<T, IS_WHITE>(callback, from, to);
                    self.pieces[piece::ALL] ^= to;
                    self.pieces[if IS_WHITE {
                        piece::WHITE_OCC
                    } else {
                        piece::BLACK_OCC
                    }] ^= to;

                    self.apply_silent_move::<IS_WHITE, PIECE>(from);
                    continue;
                }
            }
            self.apply_silent_move::<IS_WHITE, PIECE>(from | to);
            callback(self, Move::new_silent(PIECE, from, to));
            self.apply_silent_move::<IS_WHITE, PIECE>(from | to);
        }
    }

    /// capture moves to every square in moves
    #[inline]
    fn capture_loop<T: FnMut(&mut Board, Move), const IS_WHITE: bool, const PIECE: Piece>(
        &mut self,
        callback: &mut T,
        from: BitBoard,
        mut moves: BitBoard,
    ) {
        let enemy_pieces = self.copy_enemies_pieces::<IS_WHITE>();
        while moves != bitboard::EMPTY {
            let to = bitboard::extract_lsb(&mut moves);

            if PIECE == piece::PAWN {
                // check for promotions
                if to & (bitboard::rank_mask(rank::RANK_1) | bitboard::rank_mask(rank::RANK_8))
                    != bitboard::EMPTY
                {
                    self.apply_silent_move::<IS_WHITE, PIECE>(from);
                    self.clear_enemy_from_square::<IS_WHITE>(to);

                    self.pieces[if IS_WHITE {
                        piece::WHITE_OCC
                    } else {
                        piece::BLACK_OCC
                    }] ^= to;
                    self.promote_piece::<T, IS_WHITE>(callback, from, to);
                    self.pieces[if IS_WHITE {
                        piece::WHITE_OCC
                    } else {
                        piece::BLACK_OCC
                    }] ^= to;

                    self.apply_silent_move::<IS_WHITE, PIECE>(from);
                    self.set_all_enemies_pieces::<IS_WHITE>(&enemy_pieces);
                    continue;
                }
            }

            self.apply_capture::<IS_WHITE, PIECE>(from, to);
            callback(self, Move::new_capture(PIECE, from, to, to));
            self.undo_capture::<IS_WHITE, PIECE>(from, to, &enemy_pieces)
        }
    }

    /// mask of every square that is controlled (attacked) by the enemy side
    #[inline]
    pub fn danger_mask<const IS_WHITE: bool>(&self) -> BitBoard {
        let king = self.get_enemy::<IS_WHITE, { piece::KING }>();
        let knights = self.get_enemy::<IS_WHITE, { piece::KNIGHT }>();
        let pawns = self.get_enemy::<IS_WHITE, { piece::PAWN }>();
        let mut queens = self.get_enemy::<IS_WHITE, { piece::QUEEN }>();
        let mut rooks = self.get_enemy::<IS_WHITE, { piece::ROOK }>();
        let mut bishops = self.get_enemy::<IS_WHITE, { piece::BISHOP }>();

        let occ = self.pieces[piece::ALL] & !self.get_friendly::<IS_WHITE, { piece::KING }>();

        let mut queen_danger = bitboard::EMPTY;
        while queens != bitboard::EMPTY {
            let queen = bitboard::extract_lsb(&mut queens);
            queen_danger |= luts::queen_moves_lut(bitboard::to_square(queen), occ);
        }

        let mut rook_danger = bitboard::EMPTY;
        while rooks != bitboard::EMPTY {
            let rook = bitboard::extract_lsb(&mut rooks);
            rook_danger |= luts::rook_moves_lut(bitboard::to_square(rook), occ);
        }

        let mut bishop_danger = bitboard::EMPTY;
        while bishops != bitboard::EMPTY {
            let bishop = bitboard::extract_lsb(&mut bishops);
            bishop_danger |= luts::bishop_moves_lut(bitboard::to_square(bishop), occ);
        }

        let king_danger = luts::KING_MOVES[bitboard::to_square(king) as usize];
        let pawn_danger = if IS_WHITE {
            pawn_attacks::<false>(pawns)
        } else {
            pawn_attacks::<true>(pawns)
        };

        let knight_danger = bitboard::compute_knight_moves(knights);

        queen_danger | rook_danger | bishop_danger | king_danger | pawn_danger | knight_danger
    }

    /// checks if the king of the given side is in a double check
    #[inline]
    pub fn double_check<const IS_WHITE: bool>(&self) -> bool {
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let king_sq = bitboard::to_square(king);

        let pawn_checkers =
            pawn_attacks::<IS_WHITE>(king) & self.get_enemy::<IS_WHITE, { piece::PAWN }>();
        let knight_checkers =
            luts::KNIGHT_MOVE[king_sq as usize] & self.get_enemy::<IS_WHITE, { piece::KNIGHT }>();

        let queens = self.get_enemy::<IS_WHITE, { piece::QUEEN }>();
        let rooks = self.get_enemy::<IS_WHITE, { piece::ROOK }>();
        let bishops = self.get_enemy::<IS_WHITE, { piece::BISHOP }>();

        // check sliding pieces
        let hv_checkers = (queens | rooks) & luts::rook_moves_lut(king_sq, self.pieces[piece::ALL]);
        let diag_checkers: BitBoard =
            (queens | bishops) & luts::bishop_moves_lut(king_sq, self.pieces[piece::ALL]);

        // if there is also a sliding checker it's a double check
        (hv_checkers | diag_checkers | pawn_checkers | knight_checkers).count_ones() >= 2
    }

    /// masks every square where a piece can move to to block a check or capture the piece giving check
    /// (if there is a double check only the king can be moved as blocking/capturing one check is not enough)
    #[inline]
    pub fn check_mask<const IS_WHITE: bool>(&self) -> BitBoard {
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let king_sq = bitboard::to_square(king);

        let pawn_mask =
            pawn_attacks::<IS_WHITE>(king) & self.get_enemy::<IS_WHITE, { piece::PAWN }>();

        let knight_mask =
            luts::KNIGHT_MOVE[king_sq as usize] & self.get_enemy::<IS_WHITE, { piece::KNIGHT }>();

        let queens = self.get_enemy::<IS_WHITE, { piece::QUEEN }>();
        let rooks = self.get_enemy::<IS_WHITE, { piece::ROOK }>();
        let bishops = self.get_enemy::<IS_WHITE, { piece::BISHOP }>();

        // get hv sliding attackers seen by king
        let mut hv_checkers: BitBoard =
            (queens | rooks) & luts::rook_moves_lut(king_sq, self.pieces[piece::ALL]);
        let mut hv_check_mask = bitboard::EMPTY;
        // calculate up to 4 check masks
        while hv_checkers != bitboard::EMPTY {
            let checker = bitboard::extract_lsb(&mut hv_checkers);
            hv_check_mask |= luts::HV_SLIDER_CHECK_MASK
                [square::two_sq_index(&bitboard::to_square(checker), &king_sq)];
        }

        // get diag sliding attackers seen by king
        let mut diag_checkers: BitBoard =
            (queens | bishops) & luts::bishop_moves_lut(king_sq, self.pieces[piece::ALL]);
        let mut diag_check_mask = bitboard::EMPTY;
        // calculate up to 4 check masks
        while diag_checkers != bitboard::EMPTY {
            let checker = bitboard::extract_lsb(&mut diag_checkers);
            diag_check_mask |= luts::DIAG_SLIDER_CHECK_MASK
                [square::two_sq_index(&bitboard::to_square(checker), &king_sq)];
        }

        let check_mask: BitBoard = pawn_mask | knight_mask | hv_check_mask | diag_check_mask;
        match check_mask.count_ones() {
            0 => u64::MAX,
            _ => check_mask,
        }
    }

    /// horizontall and vertical pin mask of the possibly pinned square
    #[inline]
    fn hv_pin_mask<const IS_WHITE: bool>(
        &self,
        possibly_pinned_sq: Square,
        occ: BitBoard,
    ) -> BitBoard {
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let king_sq = bitboard::to_square(king);

        // get enemy slider
        let queens = self.get_enemy::<IS_WHITE, { piece::QUEEN }>();
        let rooks = self.get_enemy::<IS_WHITE, { piece::ROOK }>();

        let pinner = queens | rooks;

        let seen_by_pinned = luts::rook_moves_lut(possibly_pinned_sq, occ);
        let relevant = seen_by_pinned & bitboard::rook_mask(king_sq) & (king | pinner);

        match relevant.count_ones() {
            2 => {
                if relevant & king != bitboard::EMPTY {
                    seen_by_pinned & bitboard::rook_mask(king_sq) & !king
                } else {
                    bitboard::EVERYTHING
                }
            }
            _ => bitboard::EVERYTHING,
        }
    }

    /// same as hv_pin_mask for diagonal and anti-diagonal pin mask
    #[inline]
    fn diag_pin_mask<const IS_WHITE: bool>(
        &self,
        possibly_pinned_sq: Square,
        occ: BitBoard,
    ) -> BitBoard {
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let king_sq = bitboard::to_square(king);

        // get enemy slider
        let queens = self.get_enemy::<IS_WHITE, { piece::QUEEN }>();
        let bishops = self.get_enemy::<IS_WHITE, { piece::BISHOP }>();

        let pinner = queens | bishops;

        let seen_by_pinned = luts::bishop_moves_lut(possibly_pinned_sq, occ);
        let relevant = seen_by_pinned & bitboard::bishop_mask(king_sq) & (king | pinner);

        match (relevant | king).count_ones() {
            2 => {
                if relevant & king != bitboard::EMPTY {
                    seen_by_pinned & bitboard::bishop_mask(king_sq) & !king
                } else {
                    bitboard::EVERYTHING
                }
            }
            _ => bitboard::EVERYTHING,
        }
    }

    /// pin mask of the possibly pinned square, the piece on the square can only move
    /// inside the pin mask
    #[inline]
    pub fn pin_mask<const IS_WHITE: bool>(
        &self,
        possibly_pinned_sq: Square,
        occ: BitBoard,
    ) -> BitBoard {
        self.hv_pin_mask::<IS_WHITE>(possibly_pinned_sq, occ)
            & self.diag_pin_mask::<IS_WHITE>(possibly_pinned_sq, occ)
    }

    /// callback for evey legal king move (not including castling)
    #[inline]
    pub fn king_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
    ) {
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let king_sq = bitboard::to_square(king);

        let empty = !self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();
        let danger_mask = self.danger_mask::<IS_WHITE>();

        let old_castling_rights = self.castling_rights;
        self.clear_rights::<IS_WHITE>();

        let king_moves = luts::KING_MOVES[king_sq as usize] & !danger_mask;
        let silent_king_moves = king_moves & empty;
        self.silent_moves_loop::<T, IS_WHITE, { piece::KING }>(callback, king, silent_king_moves);

        let king_captures = king_moves & enemy;
        self.capture_loop::<T, IS_WHITE, { piece::KING }>(callback, king, king_captures);

        self.castling_rights = old_castling_rights;
    }

    /// callback for all legal castling moves (castling rights have to be correct in fen string,
    /// no additional check are performed if there rook is still in the right spot)
    #[inline]
    pub fn castling_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
    ) {
        let rooks = self.get_friendly::<IS_WHITE, { piece::ROOK }>();
        let danger_mask = self.danger_mask::<IS_WHITE>();
        let all = self.pieces[piece::ALL];

        let king_rook_mask = if IS_WHITE {
            bitboard::from_square(square::H1)
        } else {
            bitboard::from_square(square::H8)
        };
        let queen_rook_mask = if IS_WHITE {
            bitboard::from_square(square::A1)
        } else {
            bitboard::from_square(square::A8)
        };

        if self.king_side_rights::<IS_WHITE>()
            && !self.king_castling_blocked::<IS_WHITE>(all, danger_mask)
            && rooks & king_rook_mask != bitboard::EMPTY
        {
            self.castle_king_side::<T, IS_WHITE>(callback);
        }
        if self.queen_side_rights::<IS_WHITE>()
            && !self.queen_castling_blocked::<IS_WHITE>(all, danger_mask)
            && rooks & queen_rook_mask != bitboard::EMPTY
        {
            self.castle_queen_side::<T, IS_WHITE>(callback);
        }
    }

    /// generate all legal pawn moves (single move, double move, capture, promotions) excluding en passant
    #[inline]
    pub fn pawn_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        check_mask: BitBoard,
    ) {
        let mut pawns = self.get_friendly::<IS_WHITE, { piece::PAWN }>();

        let all = self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();

        while pawns != bitboard::EMPTY {
            let pawn = bitboard::extract_lsb(&mut pawns);
            let pin_mask = self.pin_mask::<IS_WHITE>(bitboard::to_square(pawn), all);

            let silent_pawn_moves = pawn_moves::<IS_WHITE>(pawn, all) & pin_mask & check_mask;
            self.silent_moves_loop::<T, IS_WHITE, { piece::PAWN }>(
                callback,
                pawn,
                silent_pawn_moves,
            );

            let double_pawn_moves =
                double_pawn_moves::<IS_WHITE>(pawn, all) & pin_mask & check_mask;
            let en_passant_sq = if IS_WHITE { pawn << 8 } else { pawn >> 8 };
            self.pieces[piece::EN_PASSANT] ^= en_passant_sq;
            self.silent_moves_loop::<T, IS_WHITE, { piece::PAWN }>(
                callback,
                pawn,
                double_pawn_moves,
            );
            self.pieces[piece::EN_PASSANT] ^= en_passant_sq;

            let pawn_captures = pawn_attacks::<IS_WHITE>(pawn) & enemy & pin_mask & check_mask;
            self.capture_loop::<T, IS_WHITE, { piece::PAWN }>(callback, pawn, pawn_captures);
        }
    }

    /// generate legal en passant moves (0-2 moves)
    #[inline]
    pub fn en_passant_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        en_passant_mask: BitBoard,
        mut check_mask: BitBoard,
    ) {
        // en passant is possible and blocks check if there is one
        let king = self.get_friendly::<IS_WHITE, { piece::KING }>();
        let pawn_checks =
            pawn_attacks::<IS_WHITE>(king) & self.get_enemy::<IS_WHITE, { piece::PAWN }>();
        check_mask |= if IS_WHITE {
            pawn_checks << 8
        } else {
            pawn_checks >> 8
        } & en_passant_mask;
        if en_passant_mask & check_mask != bitboard::EMPTY {
            let all = self.pieces[piece::ALL];
            let mut pawns = self.get_friendly::<IS_WHITE, { piece::PAWN }>();

            let pawn_to_capture;
            if IS_WHITE {
                pawns &= pawn_attacks::<false>(en_passant_mask);
                pawn_to_capture = en_passant_mask >> 8;
            } else {
                pawns &= pawn_attacks::<true>(en_passant_mask);
                pawn_to_capture = en_passant_mask << 8;
            }

            let capture_sq = bitboard::to_square(pawn_to_capture);

            while pawns != bitboard::EMPTY {
                let attacker = bitboard::extract_lsb(&mut pawns);
                let attacker_sq = bitboard::to_square(attacker);

                let attacker_pin_mask =
                    self.pin_mask::<IS_WHITE>(attacker_sq, all ^ pawn_to_capture);
                let capture_pin_mask = self.pin_mask::<IS_WHITE>(capture_sq, all ^ attacker);

                // println!("{}", bitboard::to_string(attacker));
                // println!("{}", bitboard::to_string(attacker_pin_mask));
                // println!("{}", bitboard::to_string(capture_pin_mask));

                if en_passant_mask & attacker_pin_mask & capture_pin_mask != bitboard::EMPTY {
                    self.apply_silent_move::<IS_WHITE, { piece::PAWN }>(attacker | en_passant_mask);
                    self.en_passant_update::<IS_WHITE>(pawn_to_capture);

                    callback(
                        self,
                        Move::new_capture(piece::PAWN, attacker, en_passant_mask, pawn_to_capture),
                    );

                    self.apply_silent_move::<IS_WHITE, { piece::PAWN }>(attacker | en_passant_mask);
                    self.en_passant_update::<IS_WHITE>(pawn_to_capture);
                }
            }
        }
    }

    /// generate legal knight moves
    #[inline]
    pub fn knight_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        check_mask: BitBoard,
    ) {
        let all = self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();

        let mut knights = self.get_friendly::<IS_WHITE, { piece::KNIGHT }>();
        while knights != bitboard::EMPTY {
            let knight = bitboard::extract_lsb(&mut knights);
            let pin_mask = self.pin_mask::<IS_WHITE>(bitboard::to_square(knight), all);

            let knight_moves =
                luts::KNIGHT_MOVE[bitboard::to_square(knight) as usize] & check_mask & pin_mask;

            let silent_knight_moves = knight_moves & !all;
            self.silent_moves_loop::<T, IS_WHITE, { piece::KNIGHT }>(
                callback,
                knight,
                silent_knight_moves,
            );

            let knight_captures = knight_moves & enemy;
            self.capture_loop::<T, IS_WHITE, { piece::KNIGHT }>(callback, knight, knight_captures);
        }
    }

    /// generate legal bishop moves
    #[inline]
    pub fn bishop_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        check_mask: BitBoard,
    ) {
        let all = self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();

        let mut bishops = self.get_friendly::<IS_WHITE, { piece::BISHOP }>();
        while bishops != bitboard::EMPTY {
            let bishop = bitboard::extract_lsb(&mut bishops);
            let bishop_sq = bitboard::to_square(bishop);
            let pin_mask = self.pin_mask::<IS_WHITE>(bishop_sq, all);

            let bishop_moves = luts::bishop_moves_lut(bishop_sq, all) & check_mask & pin_mask;

            let silent_bishop_moves = bishop_moves & !all;
            self.silent_moves_loop::<T, IS_WHITE, { piece::BISHOP }>(
                callback,
                bishop,
                silent_bishop_moves,
            );

            let bishop_captures = bishop_moves & enemy;
            self.capture_loop::<T, IS_WHITE, { piece::BISHOP }>(callback, bishop, bishop_captures);
        }
    }

    /// generate legal rook moves (excluding castling)
    #[inline]
    pub fn rook_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        check_mask: BitBoard,
    ) {
        let all = self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();

        let mut rooks = self.get_friendly::<IS_WHITE, { piece::ROOK }>();
        while rooks != bitboard::EMPTY {
            let rook = bitboard::extract_lsb(&mut rooks);
            let rook_sq = bitboard::to_square(rook);
            let pin_mask = self.pin_mask::<IS_WHITE>(rook_sq, all);

            let rook_moves = luts::rook_moves_lut(rook_sq, all) & check_mask & pin_mask;

            let old_castling_rights = self.castling_rights;
            if IS_WHITE {
                if rook & bitboard::from_square(square::A1) != bitboard::EMPTY {
                    self.castling_rights &= !castling::WHITE_QUEEN_SIDE;
                } else if rook & bitboard::from_square(square::H1) != bitboard::EMPTY {
                    self.castling_rights &= !castling::WHITE_KING_SIDE;
                }
            } else {
                if rook & bitboard::from_square(square::A8) != bitboard::EMPTY {
                    self.castling_rights &= !castling::BLACK_QUEEN_SIDE;
                } else if rook & bitboard::from_square(square::H8) != bitboard::EMPTY {
                    self.castling_rights &= !castling::BLACK_KING_SIDE;
                }
            }

            let silent_rook_moves = rook_moves & !all;
            self.silent_moves_loop::<T, IS_WHITE, { piece::ROOK }>(
                callback,
                rook,
                silent_rook_moves,
            );

            let rook_captures = rook_moves & enemy;
            self.capture_loop::<T, IS_WHITE, { piece::ROOK }>(callback, rook, rook_captures);

            self.castling_rights = old_castling_rights;
        }
    }

    /// generate legal queen moves
    #[inline]
    pub fn queen_moves<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
        check_mask: BitBoard,
    ) {
        let all = self.pieces[piece::ALL];
        let enemy = self.get_enemy::<IS_WHITE, { piece::OCC }>();

        let mut queens = self.get_friendly::<IS_WHITE, { piece::QUEEN }>();
        while queens != bitboard::EMPTY {
            let queen = bitboard::extract_lsb(&mut queens);
            let queen_sq = bitboard::to_square(queen);
            let pin_mask = self.pin_mask::<IS_WHITE>(queen_sq, all);

            let queen_moves = luts::queen_moves_lut(queen_sq, all) & check_mask & pin_mask;

            let silent_queen_moves = queen_moves & !all;
            self.silent_moves_loop::<T, IS_WHITE, { piece::QUEEN }>(
                callback,
                queen,
                silent_queen_moves,
            );

            let queen_captures = queen_moves & enemy;
            self.capture_loop::<T, IS_WHITE, { piece::QUEEN }>(callback, queen, queen_captures);
        }
    }

    fn legal_moves_const_side<T: FnMut(&mut Board, Move), const IS_WHITE: bool>(
        &mut self,
        callback: &mut T,
    ) {
        self.switch_player();
        self.half_moves += 1;

        // save and clear en passant mask, only needed for check mask and en passant move
        // only one clear/reset needed here instead of one per move
        let en_passant_mask = self.get(piece::EN_PASSANT);
        self.set(piece::EN_PASSANT, bitboard::EMPTY);

        self.king_moves::<T, IS_WHITE>(callback);

        // if there is a double check only the king can move
        if self.double_check::<IS_WHITE>() {
            self.switch_player();
            self.half_moves -= 1;
            return;
        }

        self.castling_moves::<T, IS_WHITE>(callback);

        let check_mask = self.check_mask::<IS_WHITE>();

        self.en_passant_moves::<T, IS_WHITE>(callback, en_passant_mask, check_mask);

        self.pawn_moves::<T, IS_WHITE>(callback, check_mask);
        self.knight_moves::<T, IS_WHITE>(callback, check_mask);
        self.rook_moves::<T, IS_WHITE>(callback, check_mask);
        self.bishop_moves::<T, IS_WHITE>(callback, check_mask);
        self.queen_moves::<T, IS_WHITE>(callback, check_mask);

        // reset board
        self.set(piece::EN_PASSANT, en_passant_mask);

        self.half_moves -= 1;
        self.switch_player();
    }

    /// generate all legal moves from given board state/position
    pub fn generate_legal_moves<T: FnMut(&mut Board, Move)>(&mut self, callback: &mut T) {
        if self.white_to_play() {
            self.legal_moves_const_side::<T, true>(callback);
        } else {
            self.legal_moves_const_side::<T, false>(callback);
        }
    }
}
