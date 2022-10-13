use crate::engine::Engine;
use crate::{Board, Move};
use std::io;
use std::io::{BufRead, Write};
use std::process::exit;

pub const ENGINE_NAME: &str = "C-RUST";
pub const AUTHOR: &str = "TIMON";

pub enum GuiToEngine {
    Uci,
    Debug(bool), // on or off
    IsReady,
    SetOption(String, Option<String>), // name <id> and value <x>
    Position(PositionOptions),
    Go(GoOptions),
    Stop,
    PonderHit,
    Quit,
    // custom
    Request(CustomGet),
}

pub enum PositionOptions {
    Fen(String),
    FenMoves(String, Vec<Move>),
    StartPos,
    StartPosMoves(Vec<Move>),
}

pub enum GoOptions {
    SearchMoves(Vec<Move>),
    Ponder,
    WTime(u64),
    BTime(u64),
    WInc(u64),
    BInc(u64),
    MovesToGo(u64),
    Depth(u64),
    Nodes(u64),
    Mate(u64),
    MoveTime(u64),
    Infinite,
}

pub enum CustomGet {
    Perft(u64),
    Position,
}

pub enum EngineToGui {
    Id(String, String), // name and author
    UciOk,
    ReadyOk,
    BestMove(Move, Option<Move>), // best move and ponder move
    CopyProtection,
    Info,
    Option,
    // custom
    Send(CustomSend),
}

pub enum CustomSend {
    Perft(Vec<(Move, u64)>),
    Position(String),
}

pub fn get_next_uci_command() -> GuiToEngine {
    let mut command = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut command).unwrap();
    let mut command_parts = command.split_whitespace();
    match command_parts.next() {
        Some("uci") => GuiToEngine::Uci,
        Some("debug") => match command_parts.next() {
            Some("on") => GuiToEngine::Debug(true),
            Some("off") => GuiToEngine::Debug(true),
            _ => panic!("Unrecognized uci command {}", command),
        },
        Some("isready") => GuiToEngine::IsReady,
        Some("setoption") => {
            if command_parts.next() != Some("name") {
                panic!("Unrecognized uci command {}", command)
            }
            let mut id: String = String::new();
            loop {
                match command_parts.next() {
                    Some("value") => break,
                    Some(s) => id += s,
                    None => return GuiToEngine::SetOption(id.clone(), None),
                }
            }
            GuiToEngine::SetOption(id, Some(String::from(command_parts.as_str())))
        }
        Some("position") => match command_parts.next() {
            Some("fen") => {
                GuiToEngine::Position(PositionOptions::Fen(String::from(command_parts.as_str())))
            }
            Some("startpos") => GuiToEngine::Position(PositionOptions::StartPos),
            // TODO add positions with moves
            _ => panic!("Unrecognized uci command {}", command),
        },
        Some("go") => {
            match command_parts.next() {
                Some("searchmoves") => GuiToEngine::Go(GoOptions::SearchMoves(vec![])),
                Some("ponder") => GuiToEngine::Go(GoOptions::Ponder),
                Some("wtime") => GuiToEngine::Go(GoOptions::WTime(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("btime") => GuiToEngine::Go(GoOptions::BTime(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("winc") => GuiToEngine::Go(GoOptions::WInc(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("binc") => GuiToEngine::Go(GoOptions::BInc(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("movestogo") => GuiToEngine::Go(GoOptions::MovesToGo(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("depth") => GuiToEngine::Go(GoOptions::Depth(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("nodes") => GuiToEngine::Go(GoOptions::Nodes(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("mate") => GuiToEngine::Go(GoOptions::Mate(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("movetime") => GuiToEngine::Go(GoOptions::MoveTime(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                Some("infinite") => GuiToEngine::Go(GoOptions::Infinite),
                Some("perft") => GuiToEngine::Request(CustomGet::Perft(
                    command_parts.next().unwrap().parse().unwrap(),
                )),
                // custom commands
                _ => panic!("Unrecognized uci command {}", command),
            }
        }
        Some("stop") => GuiToEngine::Stop,
        Some("ponderhit") => GuiToEngine::PonderHit,
        Some("quit") => GuiToEngine::Quit,
        // custom
        Some("request") => match command_parts.next() {
            Some("position") => GuiToEngine::Request(CustomGet::Position),
            Some("perft") => GuiToEngine::Request(CustomGet::Perft(
                command_parts.next().unwrap().parse().unwrap(),
            )),
            _ => panic!("Unrecognized uci command {}", command),
        },
        _ => panic!("Unrecognized uci command {}", command),
    }
}

pub fn send_uci_command(command: EngineToGui) {
    match command {
        EngineToGui::Id(name, author) => println!("id name {}\nid author {}", name, author),
        EngineToGui::UciOk => println!("uciok"),
        EngineToGui::ReadyOk => println!("readyok"),
        EngineToGui::BestMove(m1, om2) => match om2 {
            None => println!("bestmove {}", m1),
            Some(m2) => println!("bestmove {} [ponder {}]", m1, m2),
        },
        EngineToGui::CopyProtection => {}
        EngineToGui::Info => {}
        EngineToGui::Option => {}
        // custom
        EngineToGui::Send(to_send) => match to_send {
            CustomSend::Position(fen) => println!("send position {}", fen),
            CustomSend::Perft(counts) => {
                for (m, c) in &counts {
                    println!("{}: {}", m.to_uci_move(), c);
                }
                println!("\nNodes searched: {}", counts.iter().map(|(_, c)| c).sum::<u64>())
            }
        },
    };
}

pub fn processing_loop() {
    let mut engine: Engine = Engine::initialize(Board::empty_board());
    loop {
        let command = get_next_uci_command();
        match command {
            GuiToEngine::Uci => {
                send_uci_command(EngineToGui::Id(
                    String::from(ENGINE_NAME),
                    String::from(AUTHOR),
                ));
                send_uci_command(EngineToGui::UciOk);
            }
            GuiToEngine::Debug(on) => {
                engine.show_debug_information = on;
            }
            GuiToEngine::IsReady => {
                send_uci_command(EngineToGui::ReadyOk);
            }
            GuiToEngine::SetOption(_, _) => {}
            GuiToEngine::Position(pos_options) => match pos_options {
                PositionOptions::Fen(fen_str) => {
                    engine = Engine::initialize(Board::from_fen(&*fen_str))
                }
                PositionOptions::StartPos => engine = Engine::initialize(Board::starting_pos()),
                _ => {}
            },
            GuiToEngine::Go(instruction) => match instruction {
                _ => {}
            },
            GuiToEngine::Stop => {}
            GuiToEngine::PonderHit => {}
            GuiToEngine::Quit => {
                exit(0);
            }
            GuiToEngine::Request(request) => match request {
                CustomGet::Position => send_uci_command(EngineToGui::Send(CustomSend::Position(
                    engine.current_board.to_fen(),
                ))),
                CustomGet::Perft(depth) => {
                    let perft_results: Vec<(Move, u64)> = engine.perft_test(depth);
                    send_uci_command(EngineToGui::Send(CustomSend::Perft(perft_results)));
                }
            },
        }
    }
}
