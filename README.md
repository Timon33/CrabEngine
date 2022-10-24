# Rust bitboard chess engine

A chess engine in rust that can generate legal move using bitboards
and find good moves (some day).

## DONE
- working move generation: can calculate all legal moves from a legal position with a speed of about 22M nodes / second.

## TODO
- move gen utility functions (detect pieces pinned against square etc.)
- detect invalid positions (determine if a given fen is an invalid position)
- hashing of board positions


- refactor code
- speed up move generation


- simple ai
  - evaluation function
  - tree search
- support full uci protocol
- implement multithreading


- documentation