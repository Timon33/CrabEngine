use crate::Square;

pub type Rank = u8;

const FILE_CHARS: [char; 8] = ['1', '2', '3', '4', '5', '6', '7', '8'];

#[inline]
pub const fn from_square(sq: Square) -> Rank {
    sq & 0x38
}

#[inline]
pub fn to_char(rank: Rank) -> char {
    FILE_CHARS[rank as usize]
}

#[inline]
pub fn from_char(char: char) -> Option<Rank> {
    FILE_CHARS.iter().position(|c| c == &char).map(|x| (x << 3) as Rank)
}

pub const RANK_1: Rank = 0x0;
pub const RANK_2: Rank = 0x8;
pub const RANK_3: Rank = 0x10;
pub const RANK_4: Rank = 0x18;
pub const RANK_5: Rank = 0x20;
pub const RANK_6: Rank = 0x28;
pub const RANK_7: Rank = 0x30;
pub const RANK_8: Rank = 0x38;