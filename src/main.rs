use std::fmt;
use std::io::{self, Write};

// Core Structures

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Side { White, Black }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PieceType { Pawn, Knight, Bishop, Rook, Queen, King }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ChessPiece {
    pub piece_type: PieceType,
    pub color: Side,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GameStatus { Ongoing, Checkmate, Stalemate }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position { pub x: usize, pub y: usize }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub from: Position,
    pub to: Position,
    pub promotion: Option<PieceType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastlingRights {
    white_kingside: bool,
    white_queenside: bool,
    black_kingside: bool,
    black_queenside: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct MoveData {
    captured_piece: Option<ChessPiece>,
    old_castling_rights: CastlingRights,
    old_en_passant_target: Option<Position>,
}

impl ChessPiece {
    pub fn new(piece_type: PieceType, color: Side) -> Self { ChessPiece { piece_type, color } }

    fn to_char(&self) -> char {
        match self.color {
            Side::White => match self.piece_type {
                PieceType::Pawn => '♙', PieceType::Knight => '♘', PieceType::Bishop => '♗',
                PieceType::Rook => '♖', PieceType::Queen => '♕', PieceType::King => '♔',
            },
            Side::Black => match self.piece_type {
                PieceType::Pawn => '♟', PieceType::Knight => '♞', PieceType::Bishop => '♝',
                PieceType::Rook => '♜', PieceType::Queen => '♛', PieceType::King => '♚',
            },
        }
    }
}

// Board and Game Logic

#[derive(Clone)]
pub struct Board {
    grid: [[Option<ChessPiece>; 8]; 8],
    pub current_turn: Side,
    pub status: GameStatus,
    pub en_passant_target: Option<Position>,
    castling_rights: CastlingRights,
}

impl Board {
    pub fn new() -> Self {
        let mut grid = [[None; 8]; 8];
        let back_rank = [
            PieceType::Rook, PieceType::Knight, PieceType::Bishop, PieceType::Queen,
            PieceType::King, PieceType::Bishop, PieceType::Knight, PieceType::Rook,
        ];
        for (i, &piece_type) in back_rank.iter().enumerate() {
            grid[0][i] = Some(ChessPiece::new(piece_type, Side::Black));
            grid[7][i] = Some(ChessPiece::new(piece_type, Side::White));
        }
        for i in 0..8 {
            grid[1][i] = Some(ChessPiece::new(PieceType::Pawn, Side::Black));
            grid[6][i] = Some(ChessPiece::new(PieceType::Pawn, Side::White));
        }
        let castling_rights = CastlingRights {
            white_kingside: true, white_queenside: true,
            black_kingside: true, black_queenside: true,
        };
        Board { grid, current_turn: Side::White, status: GameStatus::Ongoing, en_passant_target: None, castling_rights }
    }

    pub fn make_move(&mut self, mv: Move) -> MoveData {
        let piece = self.grid[mv.from.y][mv.from.x].expect("make_move called with no piece at start");
        let move_data = MoveData {
            captured_piece: self.grid[mv.to.y][mv.to.x],
            old_castling_rights: self.castling_rights,
            old_en_passant_target: self.en_passant_target,
        };
        let mut new_en_passant_target = None;
        if piece.piece_type == PieceType::King && (mv.from.x as i8 - mv.to.x as i8).abs() == 2 {
            let (rook_from_x, rook_to_x) = if mv.to.x > mv.from.x { (7, 5) } else { (0, 3) };
            self.grid[mv.from.y][rook_to_x] = self.grid[mv.from.y][rook_from_x].take();
        }
        self.update_castling_rights(piece, mv.from);
        if piece.piece_type == PieceType::Pawn && (mv.from.y as i8 - mv.to.y as i8).abs() == 2 {
            new_en_passant_target = Some(Position { x: mv.from.x, y: (mv.from.y + mv.to.y) / 2 });
        }
        if piece.piece_type == PieceType::Pawn && Some(mv.to) == self.en_passant_target {
            let captured_pawn_y = match piece.color { Side::White => mv.to.y + 1, Side::Black => mv.to.y - 1 };
            self.grid[captured_pawn_y][mv.to.x] = None;
        }
        if let Some(promo_piece) = mv.promotion {
            self.grid[mv.to.y][mv.to.x] = Some(ChessPiece::new(promo_piece, piece.color));
        } else {
            self.grid[mv.to.y][mv.to.x] = self.grid[mv.from.y][mv.from.x];
        }
        self.grid[mv.from.y][mv.from.x] = None;
        self.en_passant_target = new_en_passant_target;
        self.current_turn = match self.current_turn { Side::White => Side::Black, Side::Black => Side::White };
        move_data
    }

    pub fn unmake_move(&mut self, mv: Move, data: MoveData) {
        let moved_piece = self.grid[mv.to.y][mv.to.x].expect("unmake_move called with no piece at destination");
        self.current_turn = match self.current_turn { Side::White => Side::Black, Side::Black => Side::White };
        if mv.promotion.is_some() {
            self.grid[mv.from.y][mv.from.x] = Some(ChessPiece::new(PieceType::Pawn, moved_piece.color));
        } else {
            self.grid[mv.from.y][mv.from.x] = Some(moved_piece);
        }
        self.grid[mv.to.y][mv.to.x] = data.captured_piece;
        if moved_piece.piece_type == PieceType::Pawn && Some(mv.to) == data.old_en_passant_target {
            let captured_pawn_y = match moved_piece.color { Side::White => mv.to.y + 1, Side::Black => mv.to.y - 1 };
            let opponent_color = match moved_piece.color { Side::White => Side::Black, Side::Black => Side::White };
            self.grid[captured_pawn_y][mv.to.x] = Some(ChessPiece::new(PieceType::Pawn, opponent_color));
        }
        if moved_piece.piece_type == PieceType::King && (mv.from.x as i8 - mv.to.x as i8).abs() == 2 {
            let (rook_from_x, rook_to_x) = if mv.to.x > mv.from.x { (7, 5) } else { (0, 3) };
            self.grid[mv.from.y][rook_from_x] = self.grid[mv.from.y][rook_to_x].take();
        }
        self.castling_rights = data.old_castling_rights;
        self.en_passant_target = data.old_en_passant_target;
        self.status = GameStatus::Ongoing;
    }

    pub fn generate_all_legal_moves(&mut self) -> Vec<Move> {
        let mut legal_moves = Vec::new();
        let pseudo_moves = self.generate_all_pseudo_legal_moves();
        let color_to_check = self.current_turn;
        for mv in pseudo_moves {
            let move_data = self.make_move(mv);
            if !self.is_in_check(color_to_check) {
                legal_moves.push(mv);
            }
            self.unmake_move(mv, move_data);
        }
        legal_moves
    }

    fn generate_all_pseudo_legal_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == self.current_turn {
                        let from = Position { x, y };
                        let destinations = self.get_pseudo_legal_moves(from);
                        for to in destinations {
                            if piece.piece_type == PieceType::Pawn && (to.y == 0 || to.y == 7) {
                                moves.push(Move { from, to, promotion: Some(PieceType::Queen) });
                                moves.push(Move { from, to, promotion: Some(PieceType::Rook) });
                                moves.push(Move { from, to, promotion: Some(PieceType::Bishop) });
                                moves.push(Move { from, to, promotion: Some(PieceType::Knight) });
                            } else {
                                moves.push(Move { from, to, promotion: None });
                            }
                        }
                    }
                }
            }
        }
        moves
    }

    fn update_castling_rights(&mut self, piece: ChessPiece, pos: Position) {
        if piece.piece_type == PieceType::King {
            if piece.color == Side::White {
                self.castling_rights.white_kingside = false; self.castling_rights.white_queenside = false;
            } else {
                self.castling_rights.black_kingside = false; self.castling_rights.black_queenside = false;
            }
        } else if piece.piece_type == PieceType::Rook {
            if piece.color == Side::White {
                if pos.x == 0 && pos.y == 7 { self.castling_rights.white_queenside = false; }
                else if pos.x == 7 && pos.y == 7 { self.castling_rights.white_kingside = false; }
            } else {
                if pos.x == 0 && pos.y == 0 { self.castling_rights.black_queenside = false; }
                else if pos.x == 7 && pos.y == 0 { self.castling_rights.black_kingside = false; }
            }
        }
    }

    pub fn is_in_check(&self, side: Side) -> bool {
        let king_pos = match self.find_king(side) { Some(pos) => pos, None => return true, };
        let opponent = match side { Side::White => Side::Black, Side::Black => Side::White };
        self.is_square_attacked(king_pos, opponent)
    }

    fn is_square_attacked(&self, pos: Position, by_side: Side) -> bool {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == by_side {
                        let current_pos = Position { x, y };
                        let attacks = match piece.piece_type {
                            PieceType::Pawn => self.get_pawn_attack_moves(current_pos, piece.color),
                            PieceType::Knight => self.get_knight_moves(current_pos, piece.color),
                            PieceType::King => self.get_king_attack_moves(current_pos),
                            PieceType::Bishop => self.get_sliding_attack_moves(current_pos, &[(1, 1), (1, -1), (-1, 1), (-1, -1)]),
                            PieceType::Rook => self.get_sliding_attack_moves(current_pos, &[(1, 0), (-1, 0), (0, 1), (0, -1)]),
                            PieceType::Queen => self.get_sliding_attack_moves(current_pos, &[(1, 1), (1, -1), (-1, 1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]),
                        };
                        if attacks.contains(&pos) { return true; }
                    }
                }
            }
        }
        false
    }

    fn find_king(&self, side: Side) -> Option<Position> {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.piece_type == PieceType::King && piece.color == side {
                        return Some(Position { x, y });
                    }
                }
            }
        }
        None
    }

    fn get_pseudo_legal_moves(&self, pos: Position) -> Vec<Position> {
        if let Some(piece) = self.grid[pos.y][pos.x] {
            match piece.piece_type {
                PieceType::Pawn => self.get_pawn_moves(pos, piece.color),
                PieceType::Knight => self.get_knight_moves(pos, piece.color),
                PieceType::Bishop => self.get_sliding_moves(pos, piece.color, &[(1, 1), (1, -1), (-1, 1), (-1, -1)]),
                PieceType::Rook => self.get_sliding_moves(pos, piece.color, &[(1, 0), (-1, 0), (0, 1), (0, -1)]),
                PieceType::Queen => self.get_sliding_moves(pos, piece.color, &[(1, 1), (1, -1), (-1, 1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]),
                PieceType::King => self.get_king_moves(pos, piece.color),
            }
        } else { Vec::new() }
    }

    fn get_pawn_attack_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let direction: i8 = if color == Side::White { -1 } else { 1 };
        for &dx in &[-1, 1] {
            let capture_x = pos.x as i8 + dx; let capture_y = pos.y as i8 + direction;
            if (0..8).contains(&capture_x) && (0..8).contains(&capture_y) {
                moves.push(Position { x: capture_x as usize, y: capture_y as usize });
            }
        }
        moves
    }
    
    fn get_sliding_attack_moves(&self, pos: Position, directions: &[(i8, i8)]) -> Vec<Position> {
        let mut moves = Vec::new();
        for &(dx, dy) in directions {
            let mut current_pos = pos;
            loop {
                let next_x = current_pos.x as i8 + dx; let next_y = current_pos.y as i8 + dy;
                if !(0..8).contains(&next_x) || !(0..8).contains(&next_y) { break; }
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                moves.push(next_pos);
                if self.grid[next_pos.y][next_pos.x].is_some() { break; }
                current_pos = next_pos;
            }
        }
        moves
    }

    fn get_sliding_moves(&self, pos: Position, color: Side, directions: &[(i8, i8)]) -> Vec<Position> {
        let mut moves = Vec::new();
        for &(dx, dy) in directions {
            let mut current_pos = pos;
            loop {
                let next_x = current_pos.x as i8 + dx; let next_y = current_pos.y as i8 + dy;
                if !(0..8).contains(&next_x) || !(0..8).contains(&next_y) { break; }
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                current_pos = next_pos;
                if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                    if target_piece.color != color { moves.push(next_pos); }
                    break;
                } else { moves.push(next_pos); }
            }
        }
        moves
    }

    fn get_knight_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let knight_moves: [(i8, i8); 8] = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)];
        for &(dx, dy) in &knight_moves {
            let next_x = pos.x as i8 + dx; let next_y = pos.y as i8 + dy;
            if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                    if target_piece.color != color { moves.push(next_pos); }
                } else { moves.push(next_pos); }
            }
        }
        moves
    }

    fn get_king_attack_moves(&self, pos: Position) -> Vec<Position> {
        let mut moves = Vec::new();
        for dy in -1..=1 {
            for dx in -1..=1 {
                if dx == 0 && dy == 0 { continue; }
                let next_x = pos.x as i8 + dx; let next_y = pos.y as i8 + dy;
                if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                    moves.push(Position { x: next_x as usize, y: next_y as usize });
                }
            }
        }
        moves
    }

    fn get_king_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = self.get_king_attack_moves(pos);
        
        // Add castling moves
        if !self.is_in_check(color) {
            let opponent = match color { Side::White => Side::Black, Side::Black => Side::White };
            if (color == Side::White && self.castling_rights.white_kingside) || (color == Side::Black && self.castling_rights.black_kingside) {
                if self.grid[pos.y][pos.x + 1].is_none() && self.grid[pos.y][pos.x + 2].is_none() {
                    if !self.is_square_attacked(Position { x: pos.x + 1, y: pos.y }, opponent) && !self.is_square_attacked(Position { x: pos.x + 2, y: pos.y }, opponent) {
                        moves.push(Position { x: pos.x + 2, y: pos.y });
                    }
                }
            }
            if (color == Side::White && self.castling_rights.white_queenside) || (color == Side::Black && self.castling_rights.black_queenside) {
                if self.grid[pos.y][pos.x - 1].is_none() && self.grid[pos.y][pos.x - 2].is_none() && self.grid[pos.y][pos.x - 3].is_none() {
                    if !self.is_square_attacked(Position { x: pos.x - 1, y: pos.y }, opponent) && !self.is_square_attacked(Position { x: pos.x - 2, y: pos.y }, opponent) {
                        moves.push(Position { x: pos.x - 2, y: pos.y });
                    }
                }
            }
        }
        moves
    }

    fn get_pawn_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let direction: i8 = if color == Side::White { -1 } else { 1 };
        let fwd1_y = pos.y as i8 + direction;
        if (0..8).contains(&fwd1_y) {
            let fwd1_pos = Position { x: pos.x, y: fwd1_y as usize };
            if self.grid[fwd1_pos.y][fwd1_pos.x].is_none() {
                moves.push(fwd1_pos);
                let start_rank = if color == Side::White { 6 } else { 1 };
                if pos.y == start_rank {
                    let fwd2_y = pos.y as i8 + 2 * direction;
                    let fwd2_pos = Position { x: pos.x, y: fwd2_y as usize };
                    if self.grid[fwd2_pos.y][fwd2_pos.x].is_none() { moves.push(fwd2_pos); }
                }
            }
        }
        for &dx in &[-1, 1] {
            let capture_x = pos.x as i8 + dx; let capture_y = pos.y as i8 + direction;
            if (0..8).contains(&capture_x) && (0..8).contains(&capture_y) {
                let capture_pos = Position { x: capture_x as usize, y: capture_y as usize };
                if let Some(target) = self.grid[capture_pos.y][capture_pos.x] {
                    if target.color != color { moves.push(capture_pos); }
                }
                if Some(capture_pos) == self.en_passant_target { moves.push(capture_pos); }
            }
        }
        moves
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const WHITE_SQUARE: &str = "\x1B[48;5;250m"; const BLACK_SQUARE: &str = "\x1B[48;5;240m";
        const RESET_COLOR: &str = "\x1B[0m";
        writeln!(f, "\n   a  b  c  d  e  f  g  h")?;
        for y in 0..8 {
            write!(f, "{} ", 8 - y)?;
            for x in 0..8 {
                let bg_color = if (x + y) % 2 == 0 { WHITE_SQUARE } else { BLACK_SQUARE };
                let piece_char = match self.grid[y][x] { Some(piece) => piece.to_char(), None => ' ' };
                write!(f, "{}{}{}{}", bg_color, " ", piece_char, " ")?;
            }
            writeln!(f, "{}{}", RESET_COLOR, 8 - y)?;
        }
        writeln!(f, "{}   a  b  c  d  e  f  g  h", RESET_COLOR)
    }
}

// Chess Bot

pub struct ChessBot {
    side: Side,
}

impl ChessBot {
    pub fn new(side: Side) -> Self { ChessBot { side } }

    pub fn find_best_move(&self, board: &mut Board) -> Option<Move> {
        let depth = 6; // Depth of the search
        let moves = board.generate_all_legal_moves();
        if moves.is_empty() { return None; }

        let mut best_move = moves[0];
        let mut max_eval = -i32::MAX;

        for &mv in &moves {
            let move_data = board.make_move(mv);
            let eval = self.minimax(board, depth - 1, -i32::MAX, i32::MAX, false);
            board.unmake_move(mv, move_data);
            if eval > max_eval {
                max_eval = eval;
                best_move = mv;
            }
        }
        Some(best_move)
    }

    fn minimax(&self, board: &mut Board, depth: u8, mut alpha: i32, mut beta: i32, maximizing_player: bool) -> i32 {
        if depth == 0 { return self.evaluate(board); }
        
        let moves = board.generate_all_legal_moves();
        if moves.is_empty() {
            if board.is_in_check(board.current_turn) {
                return -i32::MAX + (5 - depth as i32); 
            }
            return 0; // Stalemate
        }

        if maximizing_player {
            let mut max_eval = -i32::MAX;
            for &mv in &moves {
                let move_data = board.make_move(mv);
                let eval = self.minimax(board, depth - 1, alpha, beta, false);
                board.unmake_move(mv, move_data);
                max_eval = max_eval.max(eval);
                alpha = alpha.max(eval);
                if beta <= alpha { break; }
            }
            max_eval
        } else {
            let mut min_eval = i32::MAX;
            for &mv in &moves {
                let move_data = board.make_move(mv);
                let eval = self.minimax(board, depth - 1, alpha, beta, true);
                board.unmake_move(mv, move_data);
                min_eval = min_eval.min(eval);
                beta = beta.min(eval);
                if beta <= alpha { break; }
            }
            min_eval
        }
    }
    
    fn evaluate(&self, board: &Board) -> i32 {
        let mut score = 0;
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = board.grid[y][x] {
                    let material_value = match piece.piece_type {
                        PieceType::Pawn => 100, PieceType::Knight => 320,
                        PieceType::Bishop => 330, PieceType::Rook => 500,
                        PieceType::Queen => 900, PieceType::King => 20000,
                    };

                    let positional_value = 0;
                    
                    let total_value = material_value + positional_value;

                    if piece.color == self.side {
                        score += total_value;
                    } else {
                        score -= total_value;
                    }
                }
            }
        }
        if board.current_turn == self.side { score + 10 } else { score - 10 }
    }
}

// Main Game Loop

fn parse_move_str(input: &str) -> Result<Move, &'static str> {
    let trimmed_input = input.trim().to_lowercase();
    let chars: Vec<char> = trimmed_input.chars().collect();
    if !(4..=5).contains(&chars.len()) { return Err("Invalid input length. Use format 'e2e4' or 'e7e8q'."); }
    let from_x = (chars[0] as u8).wrapping_sub(b'a') as usize;
    let to_x = (chars[2] as u8).wrapping_sub(b'a') as usize;
    let from_y = 8_usize.wrapping_sub(chars[1].to_digit(10).unwrap_or(9) as usize);
    let to_y = 8_usize.wrapping_sub(chars[3].to_digit(10).unwrap_or(9) as usize);
    if from_x > 7 || to_x > 7 || from_y > 7 || to_y > 7 { return Err("Invalid coordinate. Files 'a'-'h', ranks '1'-'8'."); }
    let from = Position { x: from_x, y: from_y };
    let to = Position { x: to_x, y: to_y };
    let promotion = if chars.len() == 5 {
        let promo_char = chars[4];
        let piece_type = match promo_char {
            'q' => PieceType::Queen, 'r' => PieceType::Rook,
            'b' => PieceType::Bishop, 'n' => PieceType::Knight,
            _ => return Err("Invalid promotion character. Use q, r, b, or n."),
        };
        Some(piece_type)
    } else { None };
    Ok(Move { from, to, promotion })
}

fn main() {
    let mut board = Board::new();
    let mut message: Option<String> = None;
    let bot = ChessBot::new(Side::Black);

    loop {
        print!("\x1B[2J\x1B[1;1H");
        println!("{}", board);
        if let Some(msg) = &message { println!("\nInfo: {}", msg); }
        
        if board.status != GameStatus::Ongoing {
            println!("\n--- GAME OVER ---");
            match board.status {
                GameStatus::Checkmate => {
                    let winner = match board.current_turn { Side::White => "Black", Side::Black => "White" };
                    println!("Checkmate! {} wins.", winner);
                }
                GameStatus::Stalemate => println!("Stalemate!"),
                _ => {}
            }
            break;
        }

        if board.is_in_check(board.current_turn) { println!("\n{:?} is in CHECK!", board.current_turn); }
        
        if board.current_turn == bot.side {
            println!("\n{:?}'s turn. Bot is thinking...", board.current_turn);
            
            let best_move_option = bot.find_best_move(&mut board);

            if let Some(best_move) = best_move_option {
                let _ = board.make_move(best_move);
                let from_str = format!("{}{}", (b'a' + best_move.from.x as u8) as char, 8 - best_move.from.y);
                let to_str = format!("{}{}", (b'a' + best_move.to.x as u8) as char, 8 - best_move.to.y);
                message = Some(format!("Bot moved from {} to {}", from_str, to_str));
            } else {
                 board.status = if board.is_in_check(board.current_turn) { GameStatus::Checkmate } else { GameStatus::Stalemate };
            }
        } else {
            println!("\n{:?}'s turn.", board.current_turn);
            print!("Enter move (e.g. e2e4) or 'quit': ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            if input.trim() == "quit" { break; }
            match parse_move_str(&input) {
                Ok(mv) => {
                    let legal_moves = board.generate_all_legal_moves();
                    if legal_moves.contains(&mv) {
                        let _ = board.make_move(mv);
                        message = None;
                    } else {
                        message = Some("That is not a legal move.".to_string());
                    }
                },
                Err(e) => message = Some(e.to_string()),
            }
        }
        
        // Check game status at the end of the turn.
        let mut temp_board = board.clone();
        if temp_board.generate_all_legal_moves().is_empty() {
             if temp_board.is_in_check(temp_board.current_turn) {
                board.status = GameStatus::Checkmate;
            } else {
                board.status = GameStatus::Stalemate;
            }
        }
    }
}
