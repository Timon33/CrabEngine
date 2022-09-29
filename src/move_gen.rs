use std::arch::x86_64::_pext_u64;
use crate::{BitBoard, bitboard, board, Board, luts, piece, Square};
use crate::luts::{BISHOP_OFFSET, KING_MOVES, ROOK_OFFSET};

pub fn rook_moves(board: &Board) -> Vec<Move> {
    let mut res: Vec<Move> = Vec::new();
    let current_side = board.current_player;
    let rooks = board.board_occupation[piece::ROOK | current_side];
    let occ = board.board_occupation[piece::WHITE_OCC] | board.board_occupation[piece::BLACK_OCC];

    for rook in rooks {
        let sq = rook.as_square();
        let moves;
        unsafe {

            let mask = BitBoard::rook_mask(&sq).as_u64();
            moves = luts::ROOK_MOVE[luts::ROOK_OFFSET[sq.as_usize()] + _pext_u64(occ.as_u64(), mask) as usize]
                & !board.board_occupation[current_side];
        }
        res.push(Move::new(rook, moves));
    }
    res
}

pub fn bishop_moves(board: &Board) -> Vec<Move> {
    let mut res: Vec<Move> = Vec::new();
    let current_side = board.current_player;
    let bishops = board.board_occupation[piece::BISHOP | current_side];
    let occ = board.board_occupation[piece::WHITE_OCC] | board.board_occupation[piece::BLACK_OCC];

    for bishop in bishops {
        let sq = bishop.as_square();
        let moves;
        unsafe {
            let mask = BitBoard::bishop_mask(&sq).as_u64();
            moves = luts::BISHOP_MOVE[luts::BISHOP_OFFSET[sq.as_usize()] + _pext_u64(occ.as_u64(), mask) as usize]
                & !board.board_occupation[current_side];
        }
        res.push(Move::new(bishop, moves));
    }
    res
}

pub fn queen_moves(board: &Board) -> Vec<Move> {
    let current_side = board.current_player;
    let queen = board.board_occupation[piece::QUEEN | current_side];
    let occ = board.board_occupation[piece::WHITE_OCC] | board.board_occupation[piece::BLACK_OCC];

    let sq = queen.as_square();
    let moves;
    unsafe {
        let bishop_mask = BitBoard::bishop_mask(&sq).as_u64();
        let rook_mask = BitBoard::rook_mask(&sq).as_u64();
        moves = (luts::BISHOP_MOVE[luts::BISHOP_OFFSET[sq.as_usize()] + _pext_u64(occ.as_u64(), bishop_mask) as usize]
            | luts::ROOK_MOVE[luts::ROOK_OFFSET[sq.as_usize()] + _pext_u64(occ.as_u64(), rook_mask) as usize])
            & !board.board_occupation[current_side];
        vec![Move::new(queen, moves)]
    }
}

pub fn king_moves(board: &Board) -> Vec<Move> {
    let current_side = board.current_player;
    let king = board.board_occupation[piece::KING | current_side];

    let moves = luts::KING_MOVES[king.as_square().as_usize()] & !board.board_occupation[current_side];
    vec![(Move::new(king, moves))]
}

pub fn knight_moves(board: &Board) -> Vec<Move> {
    let mut res: Vec<Move> = Vec::new();
    let current_side = board.current_player;
    let knights = board.board_occupation[piece::KNIGHT | current_side];

    for knight in knights {
        let sq = knight.as_square();
        let moves = luts::KNIGHT_MOVE[sq.as_usize()] & !board.board_occupation[current_side];
        res.push(Move::new(knight, moves));
    }
    res
}

pub fn white_pawn_moves(board: &Board) -> Vec<Move> {
    let mut res: Vec<Move> = Vec::new();
    let pawns = board.board_occupation[piece::WHITE_PAWN];
    let occ = board.board_occupation[piece::WHITE_OCC] | board.board_occupation[piece::BLACK_OCC];

    for pawn in pawns {
        let shift_up = pawn.shift_up(1);
        let mut moves = shift_up & !occ;
        moves |= (shift_up.shift_left(1) | shift_up.shift_right(1))
            & (board.board_occupation[piece::BLACK_SIDE] | board.en_passant_mask);

        if pawn & piece::STARTING_WHITE_PAWNS != BitBoard::EMPTY {
            moves |= pawn.shift_up(2) & !(occ | occ.shift_up(1));
        }
        res.push(Move::new(pawn, moves));
    }
    res
}

pub fn black_pawn_moves(board: &Board) -> Vec<Move> {
    let mut res: Vec<Move> = Vec::new();
    let pawns = board.board_occupation[piece::BLACK_PAWN];
    let occ = board.board_occupation[piece::WHITE_OCC] | board.board_occupation[piece::BLACK_OCC];

    for pawn in pawns {
        let shift_down = pawn.shift_down(1);
        let mut moves = shift_down & !occ;
        moves |= (shift_down.shift_left(1) | shift_down.shift_right(1))
            & (board.board_occupation[piece::WHITE_SIDE] | board.en_passant_mask);

        if pawn & piece::STARTING_BLACK_PAWNS != BitBoard::EMPTY {
            moves |= pawn.shift_down(2) & !(occ | occ.shift_down(1));
        }
        res.push(Move::new(pawn, moves));
    }
    res
}

#[inline(always)]
pub fn pawn_attacks(board: &Board) {
    let pawn_attacks = if IsWhite {
        (board.w_pawns << 7) & 0x7F7F7F7F7F7F7F7F | (board.w_pawns << 9) & 0xFEFEFEFEFEFEFEFE
    } else {
        (board.b_pawns >> 9) & 0x7F7F7F7F7F7F7F7F | (board.b_pawns >> 7) & 0xFEFEFEFEFEFEFEFE
    };
}

#[inline(always)]
pub fn rook_attacks(sq: &Square, occ: BitBoard) -> BitBoard {
    unsafe {
        luts::ROOK_MOVE[ROOK_OFFSET[sq as usize] + bitboard::pext(occ, bitboard::rook_mask(sq)) as usize]
    }
}

#[inline(always)]
pub fn bishop_attacks(sq: &Square, occ: BitBoard) -> BitBoard {
    unsafe {
        luts::BISHOP_MOVE[BISHOP_OFFSET[sq as usize] + bitboard::pext(occ, bitboard::bishop_mask(sq)) as usize]
    }
}

#[inline(always)]
pub fn queen_attacks(sq: &Square, occ: BitBoard) -> BitBoard {
    rook_attacks(sq, occ) | bishop_attacks(sq, occ)
}

/// returns bitboard with square attacked by enemy
pub fn check_mask<const IsWhite: bool>(board: &Board) -> BitBoard {
    let king_sq = if IsWhite { bitboard::as_square(&board.w_king) } else { bitboard::as_square(&board.b_king) };
    let king_rays = rook_attacks(&king_square, board.occ) | bishop_attacks(&king_square, board.occ);

    let pawn_mask = if IsWhite {
        // mask black pawns attacking white king
        ((board.w_king << 7) & 0x7F7F7F7F7F7F7F7F | (board.w_king << 9) & 0xFEFEFEFEFEFEFEFE) & board.b_pawns
    } else {
        // mask white pawn attacking black king
        ((board.b_king >> 9) & 0x7F7F7F7F7F7F7F7F | (board.b_king >> 7) & 0xFEFEFEFEFEFEFEFE) & board.w_pawns
    };

    let knight_mask = if IsWhite {
        luts::KNIGHT_MOVE[king_sq] & board.b_knight
    } else {
        luts::KNIGHT_MOVE[king_sq] & board.w_knight
    };

    let queen = if IsWhite { &board.b_queen } else { board.w_queen };
    let queen_mask = king_rays & (queen | queen_attacks(&bitboard::as_square(queen), board.occ));

    let mut rooks = if IsWhite { &board.b_rooks } else { board.w_rooks };
    let rook_sq1 = bitboard::as_square(&bitboard::extract_lsb(&mut rooks));
    let rook_sq2 = bitboard::as_square(&rooks);
    let rook_mask = king_rays & (rooks | rook_attacks(&rook_sq1, board.occ) | rook_attacks(&rook_sq2, board.occ));

    let mut bishops = if IsWhite { &board.b_bishops } else { board.w_bishops };
    let bishop_sq1 = bitboard::as_square(&bitboard::extract_lsb(&mut bishops));
    let bishop_sq2 = bitboard::as_square(&bishops);
    let bishop_mask = king_rays & (bishops | rook_attacks(&bishop_sq1, board.occ) | rook_attacks(&bishop_sq2, board.occ));

    pawn_mask | knight_mask | queen_mask | rook_mask | bishop_mask
}

pub fn legal_moves(board: &Board) {
    let pawn_moves;
    if board.current_player == piece::WHITE_SIDE {
        pawn_moves = white_pawn_moves(&board);
    } else {
        pawn_moves = black_pawn_moves(&board);
    }

    let knight_moves = knight_moves(&board);
    let king_moves  = king_moves(&board);

    let rook_moves = rook_moves(&board);
    let bishop_moves = bishop_moves(&board);
    let queen_moves = queen_moves(&board);

    // if the current side is in check only allow moves that:
    // - move the king out of check
    // - block the queen/rook/bishop attacking
    // - capture the attacking piece
    if board.in_check {

    }
}



