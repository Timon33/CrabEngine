
pub type Player = u8;
pub const WHITE: Player = 0;
pub const BLACK: Player = 8;

#[inline]
pub const  fn switch_player(player: Player) -> Player {
    player ^ 8
}