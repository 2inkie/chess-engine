use std::fmt;
use std::io::{self, Write};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Side{
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

#[derive(Clone)] // We need to clone the board to check for future states
pub struct Board {
    grid: [[Option<ChessPiece>; 8]; 8],
    pub current_turn: Side,
    pub status: GameStatus,
}

impl Board {
    pub fn new() -> Self {
        let mut grid = [[None; 8]; 8];

        let back_rank = [
            PieceType::Rook,
            PieceType::Knight,
            PieceType::Bishop,
            PieceType::Queen,
            PieceType::King,
            PieceType::Bishop,
            PieceType::Knight,
            PieceType::Rook,
        ];

        for (i, &piece_type) in back_rank.iter().enumerate() {
            // Black's back rank pieces
            grid[0][i] = Some(ChessPiece::new(piece_type, Side::Black));
            // White's back rank pieces
            grid[7][i] = Some(ChessPiece::new(piece_type, Side::White));
        }

        for i in 0..8 {
            // Place Black's pawns
            grid[1][i] = Some(ChessPiece::new(PieceType::Pawn, Side::Black));
            // Place White's pawns
            grid[6][i] = Some(ChessPiece::new(PieceType::Pawn, Side::White));
        }

        Board { grid, current_turn: Side::White, status: GameStatus::Ongoing }
    }

    pub fn make_move(&mut self, from: Position, to: Position) -> Result<(), &'static str> {
        let piece = self.grid[from.y][from.x].ok_or("There is no piece at the starting square.")?;

        if piece.color != self.current_turn {
            return Err("You cannot move your opponent's piece.");
        }

        let legal_moves = self.get_legal_moves(from);
        if !legal_moves.contains(&to) {
            return Err("This is not a legal move for that piece.");
        }

        self.grid[to.y][to.x] = self.grid[from.y][from.x].take();

        // Switch turns and update game status
        self.current_turn = match self.current_turn {
            Side::White => Side::Black,
            Side::Black => Side::White,
        };
        self.update_game_status();

        Ok(())
    }

    pub fn get_legal_moves(&self, pos: Position) -> Vec<Position> {
        let pseudo_legal_moves = self.get_pseudo_legal_moves(pos);
        let piece_color = self.grid[pos.y][pos.x].unwrap().color;
        let mut legal_moves = Vec::new();

        for &mov in &pseudo_legal_moves {
            let mut temp_board = self.clone();
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
            None => return false, // Should not happen in a real game
        };

        let opponent = match side {
            Side::White => Side::Black,
            Side::Black => Side::White,
        };

        self.is_square_attacked(king_pos, opponent)
    }

    fn is_square_attacked(&self, pos: Position, by_side: Side) -> bool {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == by_side {
                        let moves = self.get_pseudo_legal_moves(Position { x, y });
                        if moves.contains(&pos) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn update_game_status(&mut self) {
        let has_legal_moves = self.has_any_legal_move(self.current_turn);

        if !has_legal_moves {
            if self.is_in_check(self.current_turn) {
                self.status = GameStatus::Checkmate;
            } else {
                self.status = GameStatus::Stalemate;
            }
        }
    }

    fn has_any_legal_move(&self, side: Side) -> bool {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.grid[y][x] {
                    if piece.color == side {
                        if !self.get_legal_moves(Position { x, y }).is_empty() {
                            return true;
                        }
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
        } else {
            Vec::new()
        }
    }

    fn get_sliding_moves(&self, pos: Position, color: Side, directions: &[(i8, i8)]) -> Vec<Position> {
        let mut moves = Vec::new();
        for &(dx, dy) in directions {
            let mut current_pos = pos;
            loop {
                let next_x = current_pos.x as i8 + dx;
                let next_y = current_pos.y as i8 + dy;

                // Check if the move is off the board
                if !(0..8).contains(&next_x) || !(0..8).contains(&next_y) {
                    break;
                }
                
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                current_pos = next_pos;

                if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                    if target_piece.color != color {
                        moves.push(next_pos);
                    }
                    break;
                } else {
                    moves.push(next_pos);
                }
            }
        }
        moves
    }

    fn get_knight_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let knight_moves: [(i8, i8); 8] = [
            (1, 2), (1, -2), (-1, 2), (-1, -2),
            (2, 1), (2, -1), (-2, 1), (-2, -1),
        ];

        for &(dx, dy) in &knight_moves {
            let next_x = pos.x as i8 + dx;
            let next_y = pos.y as i8 + dy;

            if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                let next_pos = Position { x: next_x as usize, y: next_y as usize };
                if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                    if target_piece.color != color {
                        moves.push(next_pos); // Capture
                    }
                } else {
                    moves.push(next_pos); // Empty square
                }
            }
        }
        moves
    }

    // King moves
    fn get_king_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        for dy in -1..=1 {
            for dx in -1..=1 {
                if dx == 0 && dy == 0 { continue; }

                let next_x = pos.x as i8 + dx;
                let next_y = pos.y as i8 + dy;

                if (0..8).contains(&next_x) && (0..8).contains(&next_y) {
                    let next_pos = Position { x: next_x as usize, y: next_y as usize };
                    if let Some(target_piece) = self.grid[next_pos.y][next_pos.x] {
                        if target_piece.color != color {
                            moves.push(next_pos); // Capture
                        }
                    } else {
                        moves.push(next_pos); // Empty square
                    }
                }
            }
        }
        moves
    }

    // Pawn
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
                    if self.grid[fwd2_pos.y][fwd2_pos.x].is_none() {
                        moves.push(fwd2_pos);
                    }
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
            }
        }
        moves
    }
}

// Need to add pieces as some sort of figures made of slashes or smth but for now this does the job...
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "  a b c d e f g h")?;
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
        writeln!(f, "  a b c d e f g h")
    }
}

// Parse moves
fn parse_move(input: &str) -> Result<(Position, Position), &'static str> {
    let chars: Vec<char> = input.trim().to_lowercase().chars().collect();
    if chars.len() != 4 {
        return Err("Invalid input length. Use format like 'e2e4'.");
    }

    let from_x = (chars[0] as u8).wrapping_sub(b'a') as usize;
    let to_x = (chars[2] as u8).wrapping_sub(b'a') as usize;

    let from_y = 8_usize.wrapping_sub(chars[1].to_digit(10).unwrap_or(9) as usize);
    let to_y = 8_usize.wrapping_sub(chars[3].to_digit(10).unwrap_or(9) as usize);

    if from_x > 7 || to_x > 7 || from_y > 7 || to_y > 7 {
        return Err("Invalid coordinate. Files must be 'a'-'h', ranks '1'-'8'.");
    }

    Ok((Position { x: from_x, y: from_y }, Position { x: to_x, y: to_y }))
}
fn main() {
    let mut board = Board::new();
    let mut message: Option<String> = None;

    loop {
        print!("\x1B[2J\x1B[1;1H");
        println!("CHESS");
        println!("{}", board);
        
        if let Some(msg) = &message {
            println!("\nInfo: {}", msg);
        }

        // Check game status before asking for a move
        if board.status != GameStatus::Ongoing {
            println!("\n--- GAME OVER ---");
            match board.status {
                GameStatus::Checkmate => {
                    let winner = match board.current_turn {
                        Side::White => "Black",
                        Side::Black => "White",
                    };
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

        println!("\n{:?}'s turn.", board.current_turn);
        print!("Enter move (e.g. e2e4) or 'quit': ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim() == "quit" {
            break;
        }

        match parse_move(&input) {
            Ok((from, to)) => {
                match board.make_move(from, to) {
                    Ok(()) => message = None,
                    Err(e) => message = Some(e.to_string()),
                }
            }
            Err(e) => message = Some(e.to_string()),
        }
    }
}