use crate::{file, File, rank, Rank, Square};
use std::arch::x86_64::{_pdep_u64, _pext_u64};

const SET_BIT_DISPLAY: &str = "X ";
const UNSET_BIT_DISPLAY: &str = ". ";

/// Bitboard, LSB is A1 and MSB is H8
pub type BitBoard = u64;

// public constants
pub const EMPTY: BitBoard = 0;
pub const EVERYTHING: BitBoard = u64::MAX;

// privat constants
const FILE_MASK: [BitBoard; 8] = [
    0x0101010101010101,
    0x0202020202020202,
    0x0404040404040404,
    0x0808080808080808,
    0x1010101010101010,
    0x2020202020202020,
    0x4040404040404040,
    0x8080808080808080,
];

const FULL_FILE_MASK: [BitBoard; 8] = [
    0xFFFFFFFFFFFFFFFF,
    0x7F7F7F7F7F7F7F7F,
    0x3F3F3F3F3F3F3F3F,
    0x1F1F1F1F1F1F1F1F,
    0x0F0F0F0F0F0F0F0F,
    0x0707070707070707,
    0x0303030303030303,
    0x0101010101010101,
];

const RANK_MASK: [BitBoard; 8] = [
    0x00000000000000FF,
    0x000000000000FF00,
    0x0000000000FF0000,
    0x00000000FF000000,
    0x000000FF00000000,
    0x0000FF0000000000,
    0x00FF000000000000,
    0xFF00000000000000,
];

const FULL_RANK_MASK: [BitBoard; 8] = [
    0x0000000000000000,
    0x00000000000000FF,
    0x000000000000FFFF,
    0x0000000000FFFFFF,
    0x00000000FFFFFFFF,
    0x000000FFFFFFFFFF,
    0x0000FFFFFFFFFFFF,
    0x00FFFFFFFFFFFFFF,
];

pub const INNER_FILES: BitBoard = 0x00FFFFFFFFFFFF00;
pub const INNER_RANKS: BitBoard = 0x7E7E7E7E7E7E7E7E;

/// from A8 to H1
pub const DIAGONAL_MASK: BitBoard = 0x102040810204080;
/// from H8 to A1
pub const ANTI_DIAGONAL_MASK: BitBoard = 0x8040201008040201;

/// all outermost squares
pub const BOARDER_MASK: BitBoard = FILE_MASK[0] | FILE_MASK[7] | RANK_MASK[0] | RANK_MASK[7];

// public functions

/// set only one bit at the given square
#[inline]
pub const fn from_square(sq: Square) -> BitBoard {
    1_u64 << sq
}

/// convert single (lowest) bit set to square
/// WARNING: will return an invalid square (64) if the bitboard is empty
#[inline]
pub const fn to_square(bb: BitBoard) -> Square {
    bb.trailing_zeros() as Square
}

// bitboard masks

#[inline]
pub const fn file_mask_of_sq(sq: Square) -> BitBoard {
    0x0101010101010101 << file::from_square(sq)
}

#[inline]
pub const fn file_mask(f: File) -> BitBoard {
    0x0101010101010101 << f
}

#[inline]
pub const fn rank_mask_of_sq(sq: Square) -> BitBoard {
    0xFF << rank::from_square(sq)
}

#[inline]
pub const fn rank_mask(r: Rank) -> BitBoard {
    0xFF << r
}

#[inline]
pub const fn inner_file_mask_of_sq(sq: Square) -> BitBoard {
    0x0001010101010100 << file::from_square(sq)
}

#[inline]
pub const fn inner_file_mask(f: File) -> BitBoard {
    0x0001010101010100 << f
}

#[inline]
pub const fn inner_rank_mask_of_sq(sq: Square) -> BitBoard {
    0x7E << rank::from_square(sq)
}

#[inline]
pub const fn inner_rank_mask(r: Rank) -> BitBoard {
    0x7E << r
}

#[inline]
pub const fn diagonal_mask(sq: Square) -> BitBoard {
    shift_vertical(DIAGONAL_MASK, ((sq >> 3) + (sq & 7)) as i8 - 7)
}

#[inline]
pub const fn anti_diagonal_mask(sq: Square) -> BitBoard {
    shift_vertical(ANTI_DIAGONAL_MASK, (sq & 7) as i8 - (sq >> 3) as i8)
}

/// shifts the bitboard the absolute value of n up if n is positive or down otherwise
#[inline]
pub const fn shift_vertical(bb: BitBoard, n: i8) -> BitBoard {
    if n > 0 {
        (bb & FULL_FILE_MASK[n as usize]) << n
    } else {
        (bb >> -n) & FULL_FILE_MASK[-n as usize]
    }
}

#[inline]
pub const fn rook_mask(sq: Square) -> BitBoard {
    file_mask_of_sq(sq) | rank_mask_of_sq(sq)
}

#[inline]
pub const fn rook_lut_mask(sq: Square) -> BitBoard {
    (inner_file_mask_of_sq(sq) | inner_rank_mask_of_sq(sq)) & !from_square(sq)
}

#[inline]
pub const fn bishop_mask(sq: Square) -> BitBoard {
    diagonal_mask(sq) | anti_diagonal_mask(sq)
}

#[inline]
pub const fn bishop_lut_mask(sq: Square) -> BitBoard {
    (diagonal_mask(sq) | anti_diagonal_mask(sq)) & !(from_square(sq) | BOARDER_MASK)
}

/// returns a ray of the squares excluding the from square but including the to square
/// the ray is a subset of both masks, if there is no ray between the square the result is empty
#[inline]
pub const fn ray_mask(
    to: BitBoard,
    to_mask: BitBoard,
    from: BitBoard,
    from_mask: BitBoard,
) -> BitBoard {
    let masked_attacker = to & from_mask;
    if masked_attacker == EMPTY {
        EMPTY
    } else if to < from {
        from.wrapping_sub(masked_attacker) & from_mask & to_mask
    } else {
        (masked_attacker.wrapping_sub(from) & from_mask & to_mask) ^ to ^ from
    }
}

// functions for computing moves (instead of looking them up)

#[inline]
const fn double_sub_xor(o: BitBoard, s: BitBoard) -> BitBoard {
    o.wrapping_sub(s) ^ (o.reverse_bits().wrapping_sub(s.reverse_bits())).reverse_bits()
}

#[inline]
const fn sliding_moves(occ: BitBoard, mask: BitBoard, sq_mask: BitBoard) -> BitBoard {
    let mask_occ: BitBoard = mask & occ; // & !sq_mask;
    double_sub_xor(mask_occ, sq_mask) & mask
}

#[inline]
pub const fn compute_rook_moves(sq: Square, occ: BitBoard) -> BitBoard {
    let sq_mask = from_square(sq);
    sliding_moves(occ, rank_mask_of_sq(sq), sq_mask) | sliding_moves(occ, file_mask_of_sq(sq), sq_mask)
}

#[inline]
pub const fn compute_bishop_moves(sq: Square, occ: BitBoard) -> BitBoard {
    let sq_mask = from_square(sq);
    sliding_moves(occ, diagonal_mask(sq), sq_mask)
        | sliding_moves(occ, anti_diagonal_mask(sq), sq_mask)
}

#[inline]
pub const fn compute_knight_moves(knights: BitBoard) -> BitBoard {
    ((knights << 15 | knights >> 17) & 0x7F7F7F7F7F7F7F7F) // 1 left
        | ((knights << 6 | knights >> 10) & 0x3F3F3F3F3F3F3F3F) // 2 left
        | ((knights << 17 | knights >> 15) & 0xFEFEFEFEFEFEFEFE) // 1 right
        | ((knights << 10 | knights >> 6) & 0xFCFCFCFCFCFCFCFC) // 2 right
}

#[inline]
pub fn extract_lsb(bb: &mut BitBoard) -> BitBoard {
    let lsb = *bb & ((0_u64).wrapping_sub(*bb));
    *bb ^= lsb;
    lsb
}

/// parallel bit extractions
#[inline]
pub fn pext(bb: BitBoard, mask: BitBoard) -> BitBoard {
    unsafe { _pext_u64(bb, mask) }
}

/// parallel bit deposits
#[inline]
pub fn pdep(bb: BitBoard, mask: BitBoard) -> BitBoard {
    unsafe { _pdep_u64(bb, mask) }
}

/// software emulation of the pdep instruction to be used in constant context
/// expensive, should not be used other then in const context
pub const fn bit_deposit(mut src: BitBoard, mut mask: BitBoard) -> BitBoard {
    let mut res = 0;
    loop {
        let lowest = ((!mask).wrapping_add(1)) & mask;
        if lowest == 0 {
            break;
        }
        if src & 1 != 0 {
            res |= lowest;
        }
        mask &= !lowest;
        src >>= 1;
    }
    res
}

/// turn bitboard to a ascii printable string
pub fn to_string(bb: BitBoard) -> String {
    let mut s = String::new();
    for i in (0..8_u64).rev() {
        for j in 0..8_u64 {
            if (bb & 1_u64 << j + (i << 3)) != 0 {
                s.push_str(SET_BIT_DISPLAY);
            } else {
                s.push_str(UNSET_BIT_DISPLAY);
            }
        }
        s.push_str("\n");
    }
    s
}
