use std::fmt;

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

pub struct Board {
    // Each element is an Option<ChessPiece>
    // None - empty square
    // Some(ChessPiece) - occupied square
    grid: [[Option<ChessPiece>; 8]; 8],
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

        Board { grid }
    }

    pub fn get_valid_moves(&self, pos: Position) -> Vec<Position> {
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

    // Pawn (No en-passant or promotion yet...)
    fn get_pawn_moves(&self, pos: Position, color: Side) -> Vec<Position> {
        let mut moves = Vec::new();
        let direction: i8 = if color == Side::White { -1 } else { 1 };

        // Single move forward
        let fwd1_y = pos.y as i8 + direction;
        if (0..8).contains(&fwd1_y) {
            let fwd1_pos = Position { x: pos.x, y: fwd1_y as usize };
            if self.grid[fwd1_pos.y][fwd1_pos.x].is_none() {
                moves.push(fwd1_pos);
                
                // Double move forward
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

        // Captures
        for &dx in &[-1, 1] {
            let capture_x = pos.x as i8 + dx;
            let capture_y = pos.y as i8 + direction;

            if (0..8).contains(&capture_x) && (0..8).contains(&capture_y) {
                let capture_pos = Position { x: capture_x as usize, y: capture_y as usize };
                if let Some(target) = self.grid[capture_pos.y][capture_pos.x] {
                    if target.color != color {
                        moves.push(capture_pos);
                    }
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
fn main() {
    let board = Board::new();
    println!("Initial board state:");
    println!("{}", board);
}