use crate::{BitBoard, bitboard, Square, square};

// pre calculate all constant lookup tables used by the move generation

pub const KNIGHT_MOVE: [BitBoard; 64] = {
    let mut res: [BitBoard; 64] = [bitboard::EMPTY; 64];
    let mut sq: Square = 0;
    while sq < 64 {
        // generate bitboard by ORing up to 8 possible moves
        let sq_bb = bitboard::from_square(sq);
        res[sq as usize] = bitboard::compute_knight_moves(sq_bb);
        sq += 1;
    }
    res
};
pub const KING_MOVES: [BitBoard; 64] = {
    let mut res: [BitBoard; 64] = [bitboard::EMPTY; 64];
    let mut sq: Square = 0;
    while sq < 64 {
        // generate bitboard by ORing up to 8 possible moves
        let sq_bb = bitboard::from_square(sq);
        res[sq as usize] = sq_bb << 8 | sq_bb >> 8 // up and down
            | ((sq_bb << 7 | sq_bb >> 1| sq_bb >> 9) & 0x7F7F7F7F7F7F7F7F) // left
            | ((sq_bb << 9 | sq_bb << 1| sq_bb >> 7) & 0xFEFEFEFEFEFEFEFE); // right
        sq += 1;
    }
    res
};

// masks the relevant occupancy bits for each square (lut version of bitboard::rook_occ_mask)
pub const ROOK_MASK: [BitBoard; 64] = {
    let mut res: [BitBoard; 64] = [bitboard::EMPTY; 64];
    let mut sq: Square = 0;
    while sq < 64 {
        res[sq as usize] = bitboard::rook_lut_mask(sq);
        sq += 1;
    }
    res
};
// offset for the rook move lut index
pub const ROOK_OFFSET: [usize; 64] = {
    let mut res: [usize; 64] = [0; 64];
    let mut sq: Square = 0;
    let mut current_offset: usize = 0;
    while sq < 64 {
        res[sq as usize] = current_offset;
        current_offset += 1_usize << ROOK_MASK[sq as usize].count_ones();
        sq += 1;
    }
    res
};
// possible rook moves lut index by PEXT
pub const ROOK_MOVE: [BitBoard; 0x19000] = {
    let mut res: [BitBoard; 0x19000] = [bitboard::EMPTY; 0x19000];
    let mut sq: Square = 0;
    while sq < 64 {
        let occupation_permutations: BitBoard = 1_u64 << ROOK_MASK[sq as usize].count_ones();
        let mut curr_occupation: BitBoard = 0;
        while curr_occupation < occupation_permutations {
            res[ROOK_OFFSET[sq as usize] + curr_occupation as usize] =
                bitboard::compute_rook_moves(sq, bitboard::bit_deposit(curr_occupation, ROOK_MASK[sq as usize]));
            curr_occupation += 1;
        }
        sq += 1;
    }
    res
};
pub const HV_SLIDER_CHECK_MASK: [BitBoard; 0x1000] = {
    let mut res: [BitBoard; 0x1000] = [bitboard::EMPTY; 0x1000];
    let mut king_sq: Square = 0;
    while king_sq < 64 {
        let king = bitboard::from_square(king_sq);
        let king_mask = bitboard::rook_mask(king_sq);
        let mut attacker_sq: Square = 0;
        while attacker_sq < 64 {
            let attacker = bitboard::from_square(attacker_sq);
            let attacker_mask = bitboard::rook_mask(attacker_sq);

            let check_mask = bitboard::ray_mask(attacker, attacker_mask, king, king_mask);
            res[square::two_sq_index(&attacker_sq, &king_sq)] = check_mask;
            attacker_sq += 1;
        }
        king_sq += 1;
    }
    res
};


pub const BISHOP_MASK: [BitBoard; 64] = {
    let mut res: [BitBoard; 64] = [bitboard::EMPTY; 64];
    let mut sq: Square = 0;
    while sq < 64 {
        res[sq as usize] = bitboard::bishop_lut_mask(sq);
        sq += 1;
    }
    res
};
pub const BISHOP_OFFSET: [usize; 64] =   {
    let mut res: [usize; 64] = [0; 64];
    let mut sq: Square = 0;
    let mut current_offset: usize = 0;
    while sq < 64 {
        res[sq as usize] = current_offset;
        current_offset += 1_usize << BISHOP_MASK[sq as usize].count_ones();
        sq += 1;
    }
    res
};
pub const BISHOP_MOVE: [BitBoard; 0x3580] = {
    let mut res: [BitBoard; 0x3580] = [bitboard::EMPTY; 0x3580];
    let mut sq: Square = 0;
    while sq < 64 {
        let occupation_permutations: BitBoard = 1_u64 << BISHOP_MASK[sq as usize].count_ones();
        let mut curr_occupation: BitBoard = 0;
        while curr_occupation < occupation_permutations {
            res[BISHOP_OFFSET[sq as usize] + curr_occupation as usize] =
                bitboard::compute_bishop_moves(sq, bitboard::bit_deposit(curr_occupation, BISHOP_MASK[sq as usize]));
            curr_occupation += 1;
        }
        sq += 1;
    }
    res
};
pub const DIAG_SLIDER_CHECK_MASK: [BitBoard; 0x1000] = {
    let mut res: [BitBoard; 0x1000] = [bitboard::EMPTY; 0x1000];
    let mut king_sq: Square = 0;
    while king_sq < 64 {
        let king = bitboard::from_square(king_sq);
        let king_mask = bitboard::bishop_mask(king_sq);
        let mut attacker_sq: Square = 0;
        while attacker_sq < 64 {
            let attacker = bitboard::from_square(attacker_sq);
            let attacker_mask = bitboard::bishop_mask(attacker_sq);

            let check_mask = bitboard::ray_mask(attacker, attacker_mask, king, king_mask);
            res[square::two_sq_index(&attacker_sq, &king_sq)] = check_mask;
            attacker_sq += 1;
        }
        king_sq += 1;
    }
    res
};

// use parallel bit extraction to quickly find lookup locations of moves

#[inline]
pub fn rook_moves_lut(sq: Square, occ: BitBoard) -> BitBoard {
    ROOK_MOVE[ROOK_OFFSET[sq as usize] + bitboard::pext(occ, bitboard::rook_lut_mask(sq)) as usize]
}

#[inline]
pub fn bishop_moves_lut(sq: Square, occ: BitBoard) -> BitBoard {
    BISHOP_MOVE[BISHOP_OFFSET[sq as usize] + bitboard::pext(occ, bitboard::bishop_lut_mask(sq)) as usize]
}

#[inline]
pub fn queen_moves_lut(sq: Square, occ: BitBoard) -> BitBoard {
    rook_moves_lut(sq, occ) | bishop_moves_lut(sq, occ)
}
