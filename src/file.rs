use crate::Square;

pub type File = u8;

const FILE_CHARS: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];

#[inline]
pub const fn from_square(sq: Square) -> File {
    sq & 7
}

#[inline]
pub fn to_char(file: File) -> char {
    FILE_CHARS[file as usize]
}

#[inline]
pub fn from_char(char: char) -> Option<File> {
    FILE_CHARS.iter().position(|c| c == &char.to_lowercase().next().unwrap_or('\x00')).map(|x| x as File)
}

pub const FILE_A: File = 0;
pub const FILE_B: File = 1;
pub const FILE_C: File = 2;
pub const FILE_D: File = 3;
pub const FILE_E: File = 4;
pub const FILE_F: File = 5;
pub const FILE_G: File = 6;
pub const FILE_H: File = 7;