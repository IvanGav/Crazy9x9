#![allow(dead_code)]

//0bKIIIIIICPPP
//when a Square is passed to/returned from a function, it's expected to have the above format;
// functions have to accept Square of *any* form, and format it accordingly inside
//btw, very unnecessary and less readable code (but looks cool) =D
type Square = u16;
type Coord = (usize, usize); //where (x,y)
type Offset = (isize, isize); //where (x,y)

const PIECE_VAL_AT: u8 = 0;
const COLOR_VAL_AT: u8 = 3;
const ID_VAL_AT: u8 = 4;
const DEFENDS_VAL_AT: u8 = 10;
const PIECE_VAL_MASK: Square = 0b111 << PIECE_VAL_AT;
const COLOR_VAL_MASK: Square = 0b1 << COLOR_VAL_AT;
const ID_VAL_MASK: Square = 0b111111 << ID_VAL_AT; //64 possible unique ids; ids are represented by u8, or the id mask is u64
const DEFENDS_VAL_MASK: Square = 0b1 << DEFENDS_VAL_AT; //set to 1 if this piece is on a straight or diagonal from own king, for more efficient pin checking; set to 0 otherwise

const KNIGHT_RELATIVE_MOVES: [Offset; 8] = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)];
const KING_RELATIVE_MOVES: [Offset; 8] = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)];

fn initial_state() -> [[Square; 8]; 8] {
	return [
		[Square::cp_from(Piece::Rook, Color::White), Square::cp_from(Piece::Knight, Color::White), Square::cp_from(Piece::Bishop, Color::White), Square::cp_from(Piece::Queen, Color::White), Square::cp_from(Piece::King, Color::White), Square::cp_from(Piece::Bishop, Color::White), Square::cp_from(Piece::Knight, Color::White), Square::cp_from(Piece::Rook, Color::White)],
		[Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White), Square::cp_from(Piece::Pawn, Color::White)],
		[Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new()],
		[Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new()],
		[Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new()],
		[Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new(), Square::new()],
		[Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black), Square::cp_from(Piece::Pawn, Color::Black)],
		[Square::cp_from(Piece::Rook, Color::Black), Square::cp_from(Piece::Knight, Color::Black), Square::cp_from(Piece::Bishop, Color::Black), Square::cp_from(Piece::Queen, Color::Black), Square::cp_from(Piece::King, Color::Black), Square::cp_from(Piece::Bishop, Color::Black), Square::cp_from(Piece::Knight, Color::Black), Square::cp_from(Piece::Rook, Color::Black)]
	];
}

/// x, y -> add two numbers as isize; return usize (sum must be > 0)
///
/// x + y -> add Offset and Coord (any order or combination); return Coord (component sum must be > 0)
macro_rules! add_to_usize {
	($x:ident + $y:ident) => {(add_to_usize!($x.0, $y.0),add_to_usize!($x.1, $y.1))};
    ($x:expr, $y:expr) => {((($x as isize) + ($y as isize)) as usize)};
}

///self, at, function, name; x, y \[; only empty/only captures]
///
/// - self - board
/// - at - position of the piece
/// - f(b: &mut Board, at: Coord)->() - for each square invoke this function
/// 	- b - board on which the piece is
/// 	- at - invoked square's coordinate
/// - name - label; should be descriptive of where this function goes
/// - x - move left (-), right (+) or no horizontal movement (_)
/// - y - move up (-), down (+) or no vertical movement (_)
/// - optionally, only empty/only captures can be specified to only look for empty/capture squares
///
/// **Automatically writes/clears defends flag for pieces**
macro_rules! detect_attack {
    ($self:ident, $at:ident, $f:ident, $name:tt; $dx:tt, $dy:tt) => {
		$name: {
			let mut i: usize = 1;
			while $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].empty() {
				$f($self, (_access!{$at.0, $dx, i}, _access!{$at.1, $dy, i}));
				i += 1;
				if _compare!{$at.0, $dx, i} || _compare!{$at.1, $dy, i} {
					break $name;
				}
			}
			if $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].color() != $self.at($at).color() {
				$f($self, (_access!{$at.0, $dx, i}, _access!{$at.1, $dy, i}));
			} else if $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].piece() == Piece::King {
				$self.at_mut($at).set_defends();
			}
		}
	};
	($self:ident, $at:ident, $f:ident, $name:tt; $dx:tt, $dy:tt; only empty) => {
		$name: {
			let mut i: usize = 1;
			while $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].empty() {
				$f($self, (_access!{$at.0, $dx, i}, _access!{$at.1, $dy, i}));
				i += 1;
				if _compare!{$at.0, $dx, i} || _compare!{$at.1, $dy, i} {
					break $name;
				}
			}
			if $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].color() == $self.at($at).color() &&
				$self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].piece() == Piece::King {
				$self.at_mut($at).set_defends();
			}
		}
	};
	($self:ident, $at:ident, $f:ident, $name:tt; $dx:tt, $dy:tt; only captures) => {
		$name: {
			let mut i: usize = 1;
			while $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].empty() {
				i += 1;
				if _compare!{$at.0, $dx, i} || _compare!{$at.1, $dy, i} {
					break $name;
				}
			}
			if $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].color() != $self.at($at).color() {
				$f($self, (_access!{$at.0, $dx, i}, _access!{$at.1, $dy, i}));
			} else if $self.state[_access!{$at.1, $dy, i}][_access!{$at.0, $dx, i}].piece() == Piece::King {
				$self.at_mut($at).set_defends();
			}
		}
	};
}

macro_rules! _access {
    ($coord:expr, -, $i:ident) => {$coord - $i};
	($coord:expr, +, $i:ident) => {$coord + $i};
	($coord:expr, _, $i:ident) => {$coord};
}

macro_rules! _compare {
    ($coord:expr, -, $i:ident) => {($coord as isize) - ($i as isize) < 0};
    ($coord:expr, +, $i:ident) => {$coord + $i >= 8};
    ($coord:expr, _, $i:ident) => {false};
}

///	self.field: coord1 + coord2
/// get a square from self.field at coord1+coord2 where they may be Coord or Offset
macro_rules! sq_at {
	($self:ident.$field:ident: $coord1:ident + $coord2:ident) => {$self.$field[add_to_usize!($coord1.1, $coord2.1)][add_to_usize!($coord1.0, $coord2.0)]};
}

///	coord1 + coord2
///
/// return true when this square is on the board; false otherwise where coord1, coord2 may be Coord or Offset
macro_rules! sq_valid {
	($coord1:ident + $coord2:ident) => {($coord1.0 as isize) + ($coord2.0 as isize) >= 0 && add_to_usize!($coord1.0, $coord2.0) < 8 && ($coord1.1 as isize) + ($coord2.1 as isize) >= 0 && add_to_usize!($coord1.1, $coord2.1) < 8};
}

#[derive(Copy, Clone, PartialEq)]
enum Piece {
	Pawn, Knight, Bishop, Rook, Queen, King, None
}

impl Piece {
	pub fn of(val: Square)->Self {
		return match (val & PIECE_VAL_MASK) >> PIECE_VAL_AT {
			0 => Piece::None,
			1 => Piece::Pawn,
			2 => Piece::Knight,
			3 => Piece::Bishop,
			4 => Piece::Rook,
			5 => Piece::Queen,
			_ => Piece::King,
		};
	}
	///return the numeric representation of the piece on this square; max is 6 = 0b110 -> 3 bits
	pub fn val(&self)->Square {
		return match self {
			Piece::None => 0 << PIECE_VAL_AT,
			Piece::Pawn => 1 << PIECE_VAL_AT,
			Piece::Knight => 2 << PIECE_VAL_AT,
			Piece::Bishop => 3 << PIECE_VAL_AT,
			Piece::Rook => 4 << PIECE_VAL_AT,
			Piece::Queen => 5 << PIECE_VAL_AT,
			Piece::King => 6 << PIECE_VAL_AT,
		};
	}
}

#[derive(Copy, Clone, PartialEq)]
enum Color {
	White, Black
}

impl Color {
	pub fn of(val: Square)->Self {
		return match (val & COLOR_VAL_MASK) >> COLOR_VAL_AT {
			0 => Color::White,
			_ => Color::Black,
		};
	}
	pub fn val(&self)->Square {
		return match self {
			Color::White => 0 << COLOR_VAL_AT,
			Color::Black => 1 << COLOR_VAL_AT,
		};
	}
	pub fn opposite(&self)->Self {
		return match self {
			Color::Black => Color::White,
			Color::White => Color::Black,
		};
	}
}

trait ChessPiece {
	fn new()->Self;
	fn cp_from(piece: Piece, color: Color)->Self; //cp = chess piece
	fn cp_from_with_id(piece: Piece, color: Color, id: u8)->Self; //cp = chess piece
	fn empty(&self)->bool;
	fn piece(&self)->Piece;
	fn color(&self)->Color;
	fn id(&self)->u8;
	///set id to 'id'
	fn set_id(&mut self, id: u8);
	///return true when the 'defends' flag is set
	fn defends(&self)->bool;
	///set 'defends' flag
	fn set_defends(&mut self);
	///clear 'defends' flag
	fn clear_defends(&mut self);
}

impl ChessPiece for Square {
	fn new()->Self {
		return 0;
	}
	fn cp_from(piece: Piece, color: Color)->Self {
		return piece.val() | color.val();
	}
	fn cp_from_with_id(piece: Piece, color: Color, id: u8)->Self {
		return piece.val() | color.val() | (((id as Square) << ID_VAL_AT) & ID_VAL_MASK);
	}
	fn empty(&self)->bool {
		return (*self) == 0;
	}
	fn piece(&self)->Piece {
		return Piece::of(*self);
	}
	fn color(&self)->Color {
		return Color::of(*self);
	}
	fn id(&self)->u8 {
		return ((*self & ID_VAL_MASK) >> ID_VAL_AT) as u8;
	}
	fn set_id(&mut self, id: u8) {
		*self &= !ID_VAL_MASK;
		*self |= ((id as Square) << ID_VAL_AT) & ID_VAL_MASK;
	}
	fn defends(&self)->bool {
		return (*self & DEFENDS_VAL_MASK) != 0;
	}
	fn set_defends(&mut self) {
		*self &= !DEFENDS_VAL_MASK;
	}
	fn clear_defends(&mut self) {
		*self |= DEFENDS_VAL_MASK;
	}
}

///for both 'state' and 'attack', arr\[y]\[x]; arr\[0]\[0] is a1; arr\[0]\[7] is h1; arr\[7]\[0] is a7; etc.
struct Board {
	state: [[Square; 8]; 8],
	attack: [[u64; 8]; 8], //attacked square doesn't mean this move is legal
}

impl Board {
	pub fn new()->Self {
		let mut board = Self {
			state: initial_state(),
			attack: [[0; 8]; 8],
		};
		//init piece ids
		let count: u8 = 0;
		for rank in board.state.iter_mut() {
			for sq in rank.iter_mut() {
				sq.set_id(count);
			}
		}
		//init attack array
		for y in 0..8 {
			for x in 0..8 {
				board.set_attacked_squares((x,y));
			}
		}
		return board;
	}
	pub fn at(&self, at: Coord)->&Square {
		return &self.state[at.1][at.0];
	}
	pub fn at_mut(&mut self, at: Coord)->&mut Square {
		return &mut self.state[at.1][at.0];
	}
	pub fn attack_at(&self, at: Coord)->&u64 {
		return &self.attack[at.1][at.0];
	}
	pub fn attack_at_mut(&mut self, at: Coord)->&mut u64 {
		return &mut self.attack[at.1][at.0];
	}
	fn for_straight(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up; _, -);
		detect_attack!(self, at, f, 'down; _, +);
		detect_attack!(self, at, f, 'left; -, _);
		detect_attack!(self, at, f, 'right; +, _);
	}
	fn for_diagonal(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up_left; -, -);
		detect_attack!(self, at, f, 'down_left; -, +);
		detect_attack!(self, at, f, 'up_right; +, -);
		detect_attack!(self, at, f, 'down_right; +, +);
	}
	fn for_set(&mut self, at: Coord, moves: &[Offset], f: &dyn Fn(&mut Board, Coord)->()) {
		for off in moves.iter() {
			if sq_valid!(at + off) {
				let attacked = sq_at!(self.state: at + off);
				//cannot just check for color, because getting color of empty square is undefined
				if attacked.empty() || attacked.color() != self.at(at).color() {
					f(self, add_to_usize!(at + off));
				}
			}
		}
	}
	fn for_straight_empty(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up; _, -; only empty);
		detect_attack!(self, at, f, 'down; _, +; only empty);
		detect_attack!(self, at, f, 'left; -, _; only empty);
		detect_attack!(self, at, f, 'right; +, _; only empty);
	}
	fn for_diagonal_empty(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up_left; -, -; only empty);
		detect_attack!(self, at, f, 'down_left; -, +; only empty);
		detect_attack!(self, at, f, 'up_right; +, -; only empty);
		detect_attack!(self, at, f, 'down_right; +, +; only empty);
	}
	fn for_set_empty(&mut self, at: Coord, moves: &[Offset], f: &dyn Fn(&mut Board, Coord)->()) {
		for off in moves.iter() {
			if sq_valid!(at + off) {
				let attacked = sq_at!(self.state: at + off);
				//cannot just check for color, because getting color of empty square is undefined
				if attacked.empty() {
					f(self, add_to_usize!(at + off));
				}
			}
		}
	}
	fn for_straight_captures(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up; _, -; only captures);
		detect_attack!(self, at, f, 'down; _, +; only captures);
		detect_attack!(self, at, f, 'left; -, _; only captures);
		detect_attack!(self, at, f, 'right; +, _; only captures);
	}
	fn for_diagonal_captures(&mut self, at: Coord, f: &dyn Fn(&mut Board, Coord)->()) {
		detect_attack!(self, at, f, 'up_left; -, -; only captures);
		detect_attack!(self, at, f, 'down_left; -, +; only captures);
		detect_attack!(self, at, f, 'up_right; +, -; only captures);
		detect_attack!(self, at, f, 'down_right; +, +; only captures);
	}
	fn for_set_captures(&mut self, at: Coord, moves: &[Offset], f: &dyn Fn(&mut Board, Coord)->()) {
		for off in moves.iter() {
			if sq_valid!(at + off) {
				let attacked = sq_at!(self.state: at + off);
				//cannot just check for color, because getting color of empty square is undefined
				if (!attacked.empty()) && attacked.color() != self.at(at).color() {
					f(self, add_to_usize!(at + off));
				}
			}
		}
	}
	///clear the 'attack' squares for a piece located on 'piece_at'
	pub fn clear_attacked_squares(&mut self, piece_at: Coord) {
		let id = self.at(piece_at).id();
		let clr_mask: u64 = !(1 << id);
		match self.at(piece_at).piece() {
			Piece::Pawn => {
				//clear everything, doesn't matter if it was set
				if self.at(piece_at).color() == Color::White {
					let offsets = [(0,1),(0,2),(1,1),(-1,1)]; // v ; vv ; v< ; v>
					self.for_set(piece_at, &offsets, &|b, at|->() {
						b.attack[at.1][at.0] &= clr_mask;
					});
				} else {
					let offsets = [(0,-1),(0,-2),(1,-1),(-1,-1)]; // ^ ; ^^ ; ^< ; ^>
					self.for_set(piece_at, &offsets, &|b, at|->() {
						b.attack[at.1][at.0] &= clr_mask;
					});
				}
			},
			Piece::Knight => {
				self.for_set(piece_at, &KNIGHT_RELATIVE_MOVES, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
			},
			Piece::Bishop => {
				self.for_diagonal(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
			},
			Piece::Rook => {
				self.for_straight(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
			},
			Piece::Queen => {
				self.for_diagonal(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
				self.for_straight(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
			},
			Piece::King => {
				self.for_set(piece_at, &KING_RELATIVE_MOVES, &|b, at|->() {
					b.attack[at.1][at.0] &= clr_mask;
				});
			},
			Piece::None => return,
		}
	}
	///set the 'attack' squares for a piece located on 'piece_at'
	pub fn set_attacked_squares(&mut self, piece_at: Coord) {
		let id = self.at(piece_at).id();
		let set_mask: u64 = 1 << id;
		let color = self.at(piece_at).color();
		match self.at(piece_at).piece() {
			Piece::Pawn => {
				let start_rank: usize = if color == Color::White { 1 } else { 6 };
				let move_dir = if color == Color::White { -1 } else { 1 };
				let captures: [Offset; 2] = [(1, move_dir), (-1, move_dir)];
				self.for_set_captures(piece_at, &captures, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
				let move_up: Offset = (0, move_dir);
				if sq_valid!(piece_at + move_up) && sq_at!(self.state: piece_at + move_up).empty() {
					self.attack[add_to_usize!(piece_at.1, move_up.1)][add_to_usize!(piece_at.0, move_up.0)] |= set_mask;
					let move_two: Offset = (0, move_dir*2);
					if piece_at.1 == start_rank && sq_at!(self.state: piece_at + move_two).empty() {
						sq_at!(self.attack: piece_at + move_two) |= set_mask;
					}
				}
			},
			Piece::Knight => {
				self.for_set(piece_at, &KNIGHT_RELATIVE_MOVES, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
			},
			Piece::Bishop => {
				self.for_diagonal(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
			},
			Piece::Rook => {
				self.for_straight(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
			},
			Piece::Queen => {
				self.for_diagonal(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
				self.for_straight(piece_at, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
			},
			Piece::King => {
				self.for_set(piece_at, &KING_RELATIVE_MOVES, &|b, at|->() {
					b.attack[at.1][at.0] |= set_mask;
				});
			},
			Piece::None => return,
		}
	}
}

pub fn play() {
	let mut b1 = Board::new();
	let b2 = Board::new();
	println!("b1: {}, b2: {}", b1.state[0][0].piece().val(), b2.state[0][0].piece().val()); //should be 4 both
	b1.state[0][0] = ChessPiece::new();
	println!("b1: {}, b2: {}", b1.state[0][0].piece().val(), b2.state[0][0].piece().val()); //only b2 should be 4 both, b1 should be 0
}


/*
	a move of 'p' from 's' to 'f' is illegal when:
		en passant available (and not taken)
		after this move the king is in check
	move(p):
		reset p's 'attack' squares
		update friendly knight 'attack' on p's square (add to 'attack' if any)
		reset 'attack' squares of all straight and diagonal pieces, 'prev_a'
		if p is capturing 'c', then clear all 'attack' squares of c
		move p
		reset 'attack' squares of all straight and diagonal pieces, 'post_a'
		update friendly knight 'attack' on p's square (remove from 'attack' if any)
		calculate 'attack' squares of prev_a and post_a
		calculate 'attack' squares of p
	//assume there's a list, 'check_p', of all pieces who are checking king
	is_legal(p, destination):
		if(destination not attacked by p) return false
		if(this piece defends king)
			//check if it can move without putting king in another check
			find dx,dy between p and king
			remove p from board and save
			move long dx,dy from king until found a piece
			if(found piece attacks king)
				return false
		if(own king in check)
			if(p is the king)
				if(destination is attacked)
					return false
				return true
			else
				//p is not the king -> blocking the check?
				if(check_p.length == 2) //at max 2 pieces can be putting a king in check at the same time
					return false
				if(destination.piece.id == check_p[0].id) //capturing the attacker
					return true
				if(p between king, check_p[0]) //blocking the check; destination must be empty
					return true
				return false
		else
			//legal
			return true
	//
	p between a1, a2:
		find dx1,dy1 between p and a1
		find dx2,dy2 between p and a2
		if(dx1+dx2 == 0 && dy1+dy2 == 2)
			return true
		return false
 */
