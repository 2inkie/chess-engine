use std::fmt;
use std::io::{self, Write};

// Core Data Structures

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Side {
    White,
    Black,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ChessPiece {
    pub piece_type: PieceType,
    pub color: Side,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GameStatus {
    Ongoing,
    Checkmate,
    Stalemate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub from: Position,
    pub to: Position,
    pub promotion: Option<PieceType>,
}

#[derive(Debug, Clone, Copy)]
pub struct CastlingRights {
    white_kingside: bool,
    white_queenside: bool,
    black_kingside: bool,
    black_queenside: bool,
}

impl ChessPiece {
    pub fn new(piece_type: PieceType, color: Side) -> Self {
        ChessPiece { piece_type, color }
    }

    fn to_char(&self) -> char {
        let c = match self.piece_type {
            PieceType::Pawn => 'P',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Rook => 'R',
            PieceType::Queen => 'Q',
            PieceType::King => 'K',
        };
        match self.color {
            Side::White => c,
            Side::Black => c.to_ascii_lowercase(),
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

        Board {
            grid,
            current_turn: Side::White,
            status: GameStatus::Ongoing,
            en_passant_target: None,
            castling_rights,
        }
    }
    
    pub fn make_move(&mut self, mv: Move) -> Result<(), &'static str> {
        let piece = self.grid[mv.from.y][mv.from.x].ok_or("There is no piece at the starting square.")?;
        if piece.color != self.current_turn {
            return Err("You cannot move your opponent's piece.");
        }
        let legal_moves = self.get_legal_moves(mv.from);
        if !legal_moves.contains(&mv.to) {
            return Err("This is not a legal move for that piece.");
        }

        let mut new_en_passant_target = None;

        if piece.piece_type == PieceType::King && (mv.from.x as i8 - mv.to.x as i8).abs() == 2 {
            let (rook_from_x, rook_to_x) = if mv.to.x > mv.from.x { (7, 5) } else { (0, 3) };
            let rook_pos = Position { x: rook_from_x, y: mv.from.y };
            let rook_dest = Position { x: rook_to_x, y: mv.from.y };
            self.grid[rook_dest.y][rook_dest.x] = self.grid[rook_pos.y][rook_pos.x].take();
        }

        self.update_castling_rights_on_move(mv.from);

        if piece.piece_type == PieceType::Pawn && (mv.from.y as i8 - mv.to.y as i8).abs() == 2 {
            new_en_passant_target = Some(Position { x: mv.from.x, y: (mv.from.y + mv.to.y) / 2 });
        }

        if piece.piece_type == PieceType::Pawn && Some(mv.to) == self.en_passant_target {
            let captured_pawn_y = match piece.color { Side::White => mv.to.y + 1, Side::Black => mv.to.y - 1 };
            self.grid[captured_pawn_y][mv.to.x] = None;
        }

        let is_promotion = piece.piece_type == PieceType::Pawn && (mv.to.y == 0 || mv.to.y == 7);
        if is_promotion {
            let promo_piece = mv.promotion.ok_or("Move requires a promotion piece (q, r, b, or n).")?;
            if promo_piece == PieceType::King || promo_piece == PieceType::Pawn {
                return Err("Invalid promotion piece.");
            }
            self.grid[mv.to.y][mv.to.x] = Some(ChessPiece::new(promo_piece, piece.color));
            self.grid[mv.from.y][mv.from.x] = None;
        } else {
            if mv.promotion.is_some() {
                return Err("Promotion can only be specified for a pawn reaching the final rank.");
            }
            self.grid[mv.to.y][mv.to.x] = self.grid[mv.from.y][mv.from.x].take();
        }

        self.en_passant_target = new_en_passant_target;
        self.current_turn = match self.current_turn { Side::White => Side::Black, Side::Black => Side::White };
        self.update_game_status();

        Ok(())
    }

    // The bot will call this function to get all possible moves
    pub fn generate_all_legal_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        let side_to_move = self.current_turn;

        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == side_to_move {
                        let from = Position { x, y };
                        let legal_destinations = self.get_legal_moves(from);
                        for to in legal_destinations {
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
    fn update_castling_rights_on_move(&mut self, pos: Position) {
        if pos.y == 7 && pos.x == 4 { // White King
            self.castling_rights.white_kingside = false;
            self.castling_rights.white_queenside = false;
        } else if pos.y == 0 && pos.x == 4 { // Black King
            self.castling_rights.black_kingside = false;
            self.castling_rights.black_queenside = false;
        } else if pos.y == 7 && pos.x == 7 { // White Kingside Rook
            self.castling_rights.white_kingside = false;
        } else if pos.y == 7 && pos.x == 0 { // White Queenside Rook
            self.castling_rights.white_queenside = false;
        } else if pos.y == 0 && pos.x == 7 { // Black Kingside Rook
            self.castling_rights.black_kingside = false;
        } else if pos.y == 0 && pos.x == 0 { // Black Queenside Rook
            self.castling_rights.black_queenside = false;
        }
    }

    pub fn get_legal_moves(&self, pos: Position) -> Vec<Position> {
        let pseudo_legal_moves = self.get_pseudo_legal_moves(pos);
        let piece_color = self.grid[pos.y][pos.x].unwrap().color;
        let mut legal_moves = Vec::new();

        for &mov in &pseudo_legal_moves {
            let mut temp_board = self.clone();
            // Checking legality
            temp_board.grid[mov.y][mov.x] = temp_board.grid[pos.y][pos.x].take();
            
            if !temp_board.is_in_check(piece_color) {
                legal_moves.push(mov);
            }
        }
        legal_moves
    }

    pub fn is_in_check(&self, side: Side) -> bool {
        let king_pos = match self.find_king(side) {
            Some(pos) => pos,
            None => return true,
        };
        let opponent = match side { Side::White => Side::Black, Side::Black => Side::White };
        self.is_square_attacked(king_pos, opponent)
    }

    fn is_square_attacked(&self, pos: Position, by_side: Side) -> bool {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == by_side {
                        let attacks = match piece.piece_type {
                            PieceType::Pawn => self.get_pawn_attack_moves(Position { x, y }, piece.color),

                            PieceType::King => self.get_king_attack_moves(Position { x, y }),
                            _ => self.get_pseudo_legal_moves(Position { x, y })
                        };
                        if attacks.contains(&pos) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn update_game_status(&mut self) {
        if self.generate_all_legal_moves().is_empty() {
            if self.is_in_check(self.current_turn) {
                self.status = GameStatus::Checkmate;
            } else {
                self.status = GameStatus::Stalemate;
            }
        }
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
        } else {
            Vec::new()
        }
    }

    fn get_king_attack_moves(&self, pos: Position) -> Vec<Position> {
        let mut moves = Vec::new();
        for dy in -1..=1 {
            for dx in -1..=1 {
                if dx == 0 && dy == 0 { continue; }
                let next_x = pos.x as i8 + dx;
                let next_y = pos.y as i8 + dy;
                if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                    moves.push(Position { x: next_x as usize, y: next_y as usize });
                }
            }
        }
        moves
    }

    fn get_pawn_attack_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let direction: i8 = if color == Side::White { -1 } else { 1 };
        for &dx in &[-1, 1] {
            let capture_x = pos.x as i8 + dx;
            let capture_y = pos.y as i8 + direction;
            if (0..8).contains(&capture_x) && (0..8).contains(&capture_y) {
                moves.push(Position { x: capture_x as usize, y: capture_y as usize });
            }
        }
        moves
    }

    fn get_sliding_moves(&self, pos: Position, color: Side, directions: &[(i8, i8)]) -> Vec<Position> {
        let mut moves = Vec::new();
        for &(dx, dy) in directions {
            let mut current_pos = pos;
            loop {
                let next_x = current_pos.x as i8 + dx;
                let next_y = current_pos.y as i8 + dy;
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
            let next_x = pos.x as i8 + dx;
            let next_y = pos.y as i8 + dy;
            if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                    if target_piece.color != color { moves.push(next_pos); }
                } else { moves.push(next_pos); }
            }
        }
        moves
    }

    fn get_king_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = self.get_king_attack_moves(pos); // Start with regular moves
        if self.is_in_check(color) { return moves; }
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
            let capture_x = pos.x as i8 + dx;
            let capture_y = pos.y as i8 + direction;
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
        writeln!(f, "   a b c d e f g h")?;
        writeln!(f, " +-----------------+")?;
        for y in 0..8 {
            write!(f, "{}| ", 8 - y)?;
            for x in 0..8 {
                match self.grid[y][x] {
                    Some(piece) => write!(f, "{} ", piece.to_char())?,
                    None => write!(f, ". ")?,
                }
            }
            writeln!(f, "|{}", 8-y)?;
        }
        writeln!(f, " +-----------------+")?;
        writeln!(f, "   a b c d e f g h")
    }
}

// Chess bot

pub struct ChessBot {
    side: Side,
}

impl ChessBot {
    pub fn new(side: Side) -> Self {
        ChessBot { side }
    }

    // Find the best move
    pub fn find_best_move(&self, board: &Board) -> Option<Move> {
        // We start the search with the worst possible score for us (alpha)
        // and the best possible score for the opponent (beta)
        let alpha = -i32::MAX;
        let beta = i32::MAX;
        let depth = 3; // Look 3 moves ahead as requested.

        let moves = board.generate_all_legal_moves();
        if moves.is_empty() {
            return None;
        }

        let mut best_move = moves[0];
        let mut max_eval = -i32::MAX;

        for mv in moves {
            let mut temp_board = board.clone();
            temp_board.make_move(mv).ok()?;
            
            // Call the search function for the opponent's turn
            let eval = self.minimax(&temp_board, depth - 1, alpha, beta, false);

            if eval > max_eval {
                max_eval = eval;
                best_move = mv;
            }
        }

        Some(best_move)
    }

    // Recursive minimax search function
    fn minimax(&self, board: &Board, depth: u8, mut alpha: i32, mut beta: i32, maximizing_player: bool) -> i32 {
        if depth == 0 || board.status != GameStatus::Ongoing {
            return self.evaluate(board);
        }

        let moves = board.generate_all_legal_moves();

        if maximizing_player {
            let mut max_eval = -i32::MAX;
            for mv in moves {
                let mut temp_board = board.clone();
                temp_board.make_move(mv).unwrap();
                let eval = self.minimax(&temp_board, depth - 1, alpha, beta, false);
                max_eval = max_eval.max(eval);
                alpha = alpha.max(eval);
                if beta <= alpha {
                    break; // Beta cutoff
                }
            }
            max_eval
        } else { // Minimizing player
            let mut min_eval = i32::MAX;
            for mv in moves {
                let mut temp_board = board.clone();
                temp_board.make_move(mv).unwrap();
                let eval = self.minimax(&temp_board, depth - 1, alpha, beta, true);
                min_eval = min_eval.min(eval);
                beta = beta.min(eval);
                if beta <= alpha {
                    break; // Alpha cutoff
                }
            }
            min_eval
        }
    }

    // The evaluation function. Assigns a score to a board position
    fn evaluate(&self, board: &Board) -> i32 {
        let mut score = 0;
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = board.grid[y][x] {
                    let value = match piece.piece_type {
                        PieceType::Pawn => 100,
                        PieceType::Knight => 320,
                        PieceType::Bishop => 330,
                        PieceType::Rook => 500,
                        PieceType::Queen => 900,
                        PieceType::King => 20000,
                    };
                    if piece.color == self.side {
                        score += value;
                    } else {
                        score -= value;
                    }
                }
            }
        }
        score
    }
}

// Main Game Loop

fn parse_move_str(input: &str) -> Result<Move, &'static str> {
    let trimmed_input = input.trim().to_lowercase();
    let chars: Vec<char> = trimmed_input.chars().collect();
    if !(4..=5).contains(&chars.len()) {
        return Err("Invalid input length. Use format 'e2e4' or 'e7e8q'.");
    }

    let from_x = (chars[0] as u8).wrapping_sub(b'a') as usize;
    let to_x = (chars[2] as u8).wrapping_sub(b'a') as usize;
    let from_y = 8_usize.wrapping_sub(chars[1].to_digit(10).unwrap_or(9) as usize);
    let to_y = 8_usize.wrapping_sub(chars[3].to_digit(10).unwrap_or(9) as usize);

    if from_x > 7 || to_x > 7 || from_y > 7 || to_y > 7 {
        return Err("Invalid coordinate. Files 'a'-'h', ranks '1'-'8'.");
    }

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
    } else {
        None
    };

    Ok(Move { from, to, promotion })
}

fn main() {
    let mut board = Board::new();
    let mut message: Option<String> = None;
    
    // Create the bot for the Black side
    let bot = ChessBot::new(Side::Black);

    loop {
        print!("\x1B[2J\x1B[1;1H");
        println!("{}", board);
        
        if let Some(msg) = &message {
            println!("\nInfo: {}", msg);
        }

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

        if board.is_in_check(board.current_turn) {
            println!("\n{:?} is in CHECK!", board.current_turn);
        }

        // Check whose turn it is
        if board.current_turn == bot.side {
            // Bot's turn
            println!("\n{:?}'s turn. Bot is thinking...", board.current_turn);
            if let Some(best_move) = bot.find_best_move(&board) {
                board.make_move(best_move).unwrap();
                message = Some(format!("Bot moved from {:?} to {:?}", best_move.from, best_move.to));
            }
        } else {
            // Player's turn
            println!("\n{:?}'s turn.", board.current_turn);
            print!("Enter move (e.g. e2e4) or 'quit': ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim() == "quit" { break; }

            match parse_move_str(&input) {
                Ok(mv) => {
                    match board.make_move(mv) {
                        Ok(()) => message = None,
                        Err(e) => message = Some(e.to_string()),
                    }
                }
                Err(e) => message = Some(e.to_string()),
            }
        }
    }
}
