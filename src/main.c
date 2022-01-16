// bumchess
// Branchless Unmake/Make Chess Move Generator
// 
// Copyright (c) 2022 Dmitry Shechtman
// All rights reserved.

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum Type {
	Type_None,
	Type_King,
	Type_Pawn0,
	Type_Pawn1,
	Type_Knight,
	Type_Bishop,
	Type_Rook,
	Type_Queen,
	Type_Count
};

enum Shift {
	Shift_Castling    =  1,
	Shift_Odd         =  1,
	Shift_Type        =  2,
	Shift_Rank        =  4,

	Shift_Square      =  8,
};

enum Piece {
	Piece_Castling = 0x01,
	Piece_Odd      = 0x02,
	Piece_Index2   = 0x03,

	Piece_King     = Type_King   << Shift_Type,
	Piece_Pawn0    = Type_Pawn0  << Shift_Type,
	Piece_Pawn1    = Type_Pawn1  << Shift_Type,
	Piece_Knight   = Type_Knight << Shift_Type,
	Piece_Bishop   = Type_Bishop << Shift_Type,
	Piece_Rook     = Type_Rook   << Shift_Type,
	Piece_Queen    = Type_Queen  << Shift_Type,
	Piece_Type     = Piece_Queen,
	Piece_TypePawn = Piece_Queen ^ Piece_King,

	Piece_Black    = 0x20,
	Piece_Moved    = 0x40,
	Piece_White    = 0x80,
	Piece_Color    = Piece_Black | Piece_White,

	Piece_Type4    = Piece_Type | Piece_Black,
	Piece_Index    = Piece_Index2 | Piece_Type4,

	Piece_EP       = Piece_Black,
};

enum Square {
	Square_FileA       = 0x00,
	Square_FileE       = 0x04,
	Square_FileH       = 0x07,
	Square_File        = 0x07,
	Square_FileInvalid = 0x08,

	Square_Rank1       = 0x00,
	Square_Rank2       = 0x10,
	Square_Rank3       = 0x20,
	Square_Rank6       = 0x50,
	Square_Rank7       = 0x60,
	Square_Rank8       = 0x70,
	Square_Rank        = 0x70,
	Square_RankInvalid = 0x80,

	Square_Invalid     = Square_FileInvalid | Square_RankInvalid
};

enum PieceSquare {
	PieceSquare_Invalid = Square_FileInvalid << Shift_Square,
};

enum Vec {
	Vec_SW  = -17,
	Vec_S   = -16,
	Vec_SE  = -15,
	Vec_W   =  -1,
	Vec_E   =   1,
	Vec_NW  =  15,
	Vec_N   =  16,
	Vec_NE  =  17,
	Vec_SSW = -33,
	Vec_SSE = -31,
	Vec_SWW = -18,
	Vec_SEE = -14,
	Vec_NWW =  14,
	Vec_NEE =  18,
	Vec_NNW =  31,
	Vec_NNE =  33
};

enum Count {
	Count_Colors    =    2,
	Count_Type4     =   16,

	Count_Ranks     =    8,
	Count_Files     =    8,
	Count_Squares   =  128,

	Count_Pieces    =   64,
};

enum Max {
	Max_Pawns     =    8,
	Max_Knights   =    4,
	Max_Bishops   =    4,
	Max_Bishops2  =    2,
	Max_Rooks     =    4,
	Max_Queens    =    4,

	Max_Chars     = 1024,
	Max_Moves     = 1024,
};

enum Rook {
	Rook_A,
	Rook_H,
	Rook_Count
};

enum Castling {
	Castling_White = 0,
	Castling_Black = 2,
	Castling_Count = 4
};

typedef uint8_t piece_t;
typedef uint8_t square_t;
typedef int8_t  vector_t;

vector_t vectors[] = {
	Vec_SW,  Vec_SE,  Vec_NW,  Vec_NE,
	Vec_S,   Vec_W,   Vec_E,   Vec_N,
	Vec_SSW, Vec_SSE, Vec_SWW, Vec_SEE,
	Vec_NWW, Vec_NEE, Vec_NNW, Vec_NNE
};

typedef union {
	uint16_t value;
	struct {
		piece_t  piece;
		square_t square;
	};
} piece_square_t;

typedef struct {
	piece_square_t from;
	piece_square_t to;
} from_to_t;

typedef struct {
	from_to_t prim;
	from_to_t sec;
} move_t;

typedef struct {
	square_t square;
} ep_state_t;

typedef struct {
	uint64_t piecemask;
	ep_state_t ep;
} state_t;

enum Char {
	Char_Zero = '0',
	Char_Nine = '9',

	Char_Rank = '1',
	Char_File = 'a',
};

piece_t squares[Count_Squares];
piece_square_t pieces[Count_Pieces];
state_t state;
piece_t color;

const move_t nullmove = {
	.prim = {
		.from = { PieceSquare_Invalid },
		.to = { PieceSquare_Invalid }
	},
	.sec = {
		.from = { PieceSquare_Invalid },
		.to = { PieceSquare_Invalid }
	}
};

char piece_chars[Count_Type4] = ":KPPNBRQ;kppnbrq";

uint8_t piece_ranks[Count_Type4] = {
	Square_RankInvalid, Square_Rank1,       Square_Rank2,       Square_RankInvalid,
	Square_RankInvalid, Square_RankInvalid, Square_RankInvalid, Square_RankInvalid,
	Square_RankInvalid, Square_Rank8,       Square_Rank7,       Square_RankInvalid,
	Square_RankInvalid, Square_RankInvalid, Square_RankInvalid, Square_RankInvalid
};

char color_chars[Count_Colors]  = "wb";

piece_t  color_values[Count_Colors] = { Piece_White,  Piece_Black  };
square_t color_ranks[Count_Colors]  = { Square_Rank6, Square_Rank3 };

char castling_chars[Castling_Count] = "QKqk";

square_t castling_rooks[Castling_Count] = {
	Square_FileA | Square_Rank1, Square_FileH | Square_Rank1,
	Square_FileA | Square_Rank8, Square_FileH | Square_Rank8
};

square_t color_kings[Count_Colors] = {
	Square_FileE | Square_Rank1,
	Square_FileE | Square_Rank8
};

typedef struct {
	const char* fen;
	uint8_t  min;
	uint8_t  max;
	uint8_t  div;
	uint64_t result;
} params_t;

piece_t get_square(square_t square) {
	return squares[square];
}

void clear_square(piece_square_t ps) {
	squares[ps.square] = 0x00;
}

void set_square(piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

piece_square_t get_piece(piece_t piece) {
	return pieces[piece];
}

bool has_piece(piece_t piece) {
	return state.piecemask & (1ull << (piece & Piece_Index));
}

void clear_piece(piece_square_t ps) {
	piece_t piece = ps.piece & Piece_Index;
	state.piecemask &= ~(1ull << piece);
}

void set_piece(piece_square_t ps) {
	piece_t piece = ps.piece & Piece_Index;
	pieces[piece] = ps;
	state.piecemask |= (1ull << piece);
}

uint8_t get_index2(square_t square) {
	return ((square ^ (square >> Shift_Rank)) & 1) << Shift_Odd;
}

piece_square_t find_index_to(piece_square_t ps) {
	for (uint64_t mask = state.piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

piece_square_t find_index_to_bishop(piece_square_t ps) {
	ps.value += get_index2(ps.square);
	return find_index_to(ps);
}

piece_square_t find_index_error(piece_square_t ps) {
	ps.value = 0;
	return ps;
}

piece_square_t find_index_king(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return (ps2.value & Piece_Index2)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_pawn(piece_square_t ps) {
	ps.piece |= (ps.square & Square_File);
	return has_piece(ps.piece)
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_bishop(piece_square_t ps) {
	ps.value += get_index2(ps.square);
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & (Piece_Type | Piece_Odd))
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_rook(piece_square_t ps) {
	ps.piece |= (state.piecemask >> (Piece_King | (ps.piece & Piece_Black))) & Piece_Castling;
	return has_piece(ps.piece)
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_moved_pawn(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & Piece_TypePawn)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_other(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & Piece_Type)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index(piece_square_t ps) {
	switch (ps.piece & Piece_Type) {
	case Piece_King:
		return find_index_king(ps);
	case Piece_Pawn0:
		return find_index_pawn(ps);
	case Piece_Rook:
		return find_index_rook(ps);
	default:
		return find_index_error(ps);
	}
}

piece_square_t find_index_moved(piece_square_t ps) {
	switch (ps.piece & Piece_Type) {
	case Piece_King:
		return find_index_king(ps);
	case Piece_Pawn0:
		return find_index_moved_pawn(ps);
	case Piece_Bishop:
		return find_index_bishop(ps);
	default:
		return find_index_other(ps);
	}
}

move_t* gen_promo(move_t* moves, move_t move, piece_square_t to, piece_t piece) {
	to.piece = piece | color;
	move.prim.to = find_index_to(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_bishop(move_t* moves, move_t move, piece_square_t to) {
	to.piece = Piece_Bishop | color;
	move.prim.to = find_index_to_bishop(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to, uint8_t promo) {
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo(moves, move, to, Piece_Knight);
		moves = gen_promo_bishop(moves, move, to);
		moves = gen_promo(moves, move, to, Piece_Rook);
		moves = gen_promo(moves, move, to, Piece_Queen);
		return moves;
	}
	*moves++ = move;
	return moves;
}

move_t* gen_push_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!(from2.piece = get_square(from2.square = to.square += vector))) {
		move_t move = {
			.prim = {
				.from = from,
				.to = to
			},
			.sec = {
				.from = from2,
				.to = { PieceSquare_Invalid }
			}
		};
		moves = gen_promo_pawn(moves, move, to, promo);
		if (!(from.piece & Piece_Moved)
			&& !get_square(to.square += vector)) {
				move.prim.to = to;
				move.sec.to.value = from2.value | Piece_EP | PieceSquare_Invalid;
				*moves++ = move;
		}
	}
	return moves;
}

move_t* gen_vector_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& (from2.piece = get_square(from2.square = to.square)) & (color ^ Piece_Color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { PieceSquare_Invalid }
				}
			};
			moves = gen_promo_pawn(moves, move, to, promo);
	}
	return moves;
}

bool check_vector_pawn(square_t square, vector_t vector) {
	return !((square += vector) & Square_Invalid)
		&& (get_square(square) & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color);
}

move_t* gen_vector_ep(move_t* moves, vector_t vector) {
	piece_square_t to = {
		.square = state.ep.square & ~Square_FileInvalid
	};
	piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = get_square(from.square)) & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = {
						.piece = (to.square & Square_File) | Piece_Pawn0 | (color ^ Piece_Color) | Piece_Moved,
						.square = to.square ^ Square_Rank2
					},
					.to = { PieceSquare_Invalid }
				}
		};
		*moves++ = move;
	}
	return moves;
}

move_t* gen_vector_leaper(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(from2.square = to.square)) & color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { PieceSquare_Invalid }
				}
			};
			*moves++ = move;
	}
	return moves;
}

bool check_vector_knight(square_t square, vector_t vector) {
	return !((square += vector) & Square_Invalid)
		&& (get_square(square) & (Piece_Type | Piece_Color)) == (Piece_Knight | color);
}

move_t* gen_vector_slider(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2 = {0};
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(from2.square = to.square)) & color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { PieceSquare_Invalid }
				}
			};
			*moves++ = move;
	}
	return moves;
}

bool check_vector(square_t src, square_t dest, vector_t vector) {
	do {
		src += vector;
	} while (!get_square(src));
	return src == dest;
}

move_t* gen_slider(move_t* moves, piece_square_t from, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		moves = gen_vector_slider(moves, from, vectors[i]);
	}
	return moves;
}

bool check_vert(square_t src, square_t dest, int8_t drank) {
	return !(drank & Square_FileInvalid)
		? check_vector(src, dest, Vec_N)
		: check_vector(src, dest, Vec_S);
}

bool check_horiz(square_t src, square_t dest, int8_t dfile) {
	return !(dfile & Square_FileInvalid)
		? check_vector(src, dest, Vec_E)
		: check_vector(src, dest, Vec_W);
}

bool check_diag1(square_t src, square_t dest, int8_t drank) {
	return !(drank & Square_FileInvalid)
		? check_vector(src, dest, Vec_NE)
		: check_vector(src, dest, Vec_SW);
}

bool check_diag2(square_t src, square_t dest, int8_t dfile) {
	return !(dfile & Square_FileInvalid)
		? check_vector(src, dest, Vec_SE)
		: check_vector(src, dest, Vec_NW);
}

bool check_ortho(square_t src, square_t dest, int8_t dfile, int8_t drank) {
	return !dfile
		? check_vert(src, dest, drank)
		: !drank
			? check_horiz(src, dest, dfile)
			: false;
}

bool check_diag(square_t src, square_t dest, int8_t dfile, int8_t drank) {
	return dfile == drank
		? check_diag1(src, dest, drank)
		: dfile == -drank
			? check_diag2(src, dest, dfile)
			: false;
}

move_t* gen_pawn_white(move_t* moves, piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_NW, Square_Rank8);
	moves = gen_vector_pawn(moves, from, Vec_NE, Square_Rank8);
	return gen_push_pawn(moves, from, Vec_N, Square_Rank8);
}

move_t* gen_ep_white(move_t* moves) {
	moves = gen_vector_ep(moves, Vec_SW);
	moves = gen_vector_ep(moves, Vec_SE);
	return moves;
}

bool check_pawns_white(square_t dest) {
	return check_vector_pawn(dest, Vec_SW)
		|| check_vector_pawn(dest, Vec_SE);
}

move_t* gen_pawn_black(move_t* moves, piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_SW, Square_Rank1);
	moves = gen_vector_pawn(moves, from, Vec_SE, Square_Rank1);
	return gen_push_pawn(moves, from, Vec_S, Square_Rank1);
}

move_t* gen_ep_black(move_t* moves) {
	moves = gen_vector_ep(moves, Vec_NW);
	moves = gen_vector_ep(moves, Vec_NE);
	return moves;
}

bool check_pawns_black(square_t dest) {
	return check_vector_pawn(dest, Vec_NW)
		|| check_vector_pawn(dest, Vec_NE);
}

move_t* gen_pawn(move_t* moves, piece_square_t from) {
	return color == Piece_White
		? gen_pawn_white(moves, from)
		: gen_pawn_black(moves, from);
}

bool check_pawns(square_t dest) {
	return color == Piece_White
		? check_pawns_white(dest)
		: check_pawns_black(dest);
}

move_t* gen_ep(move_t* moves) {
	return state.piecemask & (1ull << Piece_EP)
		? color == Piece_White
			? gen_ep_white(moves)
			: gen_ep_black(moves)
		: moves;
}

move_t* gen_king(move_t* moves, piece_square_t from) {
	for (uint8_t i = 0; i < 8; ++i) {
		moves = gen_vector_leaper(moves, from, vectors[i]);
	}
	return moves;
}

bool check_king(piece_square_t from, square_t dest) {
	square_t src = from.square;
	uint8_t delta = abs(dest - src);
	return delta == Vec_E || delta == Vec_NW || delta == Vec_N || delta == Vec_NE;
}

move_t* gen_knight(move_t* moves, piece_square_t from) {
	for (uint8_t i = 8; i < 16; ++i) {
		moves = gen_vector_leaper(moves, from, vectors[i]);
	}
	return moves;
}

bool check_knights(square_t dest) {
	for (uint8_t i = 8; i < 16; ++i) {
		if (check_vector_knight(dest, vectors[i])) {
			return true;
		}
	}
	return false;
}

move_t* gen_bishop(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 0, 4);
}

bool check_bishop(piece_square_t from, square_t dest) {
	square_t src = from.square;
	int8_t dfile = (dest & Square_File) - (src & Square_File);
	int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank);
}

move_t* gen_rook(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 4, 8);
}

bool check_rook(piece_square_t from, square_t dest) {
	square_t src = from.square;
	int8_t dfile = (dest & Square_File) - (src & Square_File);
	int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_ortho(src, dest, dfile, drank);
}

move_t* gen_queen(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 0, 8);
}

bool check_queen(piece_square_t from, square_t dest) {
	square_t src = from.square;
	int8_t dfile = (dest & Square_File) - (src & Square_File);
	int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank)
		|| check_ortho(src, dest, dfile, drank);
}

move_t* gen_kings(move_t* moves) {
	piece_t piece = (Piece_King | color) & Piece_Index;
	return gen_king(moves, get_piece(piece));
}

bool check_kings(square_t dest) {
	piece_t piece = (Piece_King | color) & Piece_Index;
	return check_king(get_piece(piece), dest);
}

move_t* gen_pawns(move_t* moves) {
	piece_t piece = (Piece_Pawn0 | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Pawns; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_pawn(moves, get_piece(piece));
		}
	}
	return moves;
}

move_t* gen_knights(move_t* moves) {
	piece_t piece = (Piece_Knight | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Knights; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_knight(moves, get_piece(piece));
		}
	}
	return moves;
}

move_t* gen_bishops(move_t* moves) {
	piece_t piece = (Piece_Bishop | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Bishops; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_bishop(moves, get_piece(piece));
		}
	}
	return moves;
}

bool check_bishops(square_t dest) {
	piece_t piece = ((Piece_Bishop + get_index2(dest)) | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Bishops2; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_bishop(get_piece(piece), dest)) {
			return true;
		}
	}
	return false;
}

move_t* gen_rooks(move_t* moves) {
	piece_t piece = (Piece_Rook | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Rooks; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_rook(moves, get_piece(piece));
		}
	}
	return moves;
}

bool check_rooks(square_t dest) {
	piece_t piece = (Piece_Rook | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Rooks; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_rook(get_piece(piece), dest)) {
			return true;
		}
	}
	return false;
}

move_t* gen_queens(move_t* moves) {
	piece_t piece = (Piece_Queen | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Queens; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_queen(moves, get_piece(piece));
		}
	}
	return moves;
}

bool check_queens(square_t dest) {
	piece_t piece = (Piece_Queen | color) & Piece_Index;
	uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Max_Queens; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_queen(get_piece(piece), dest)) {
			return true;
		}
	}
	return false;
}

bool check_to(square_t dest) {
	return check_pawns(dest)
		|| check_kings(dest)
		|| check_knights(dest)
		|| check_bishops(dest)
		|| check_rooks(dest)
		|| check_queens(dest);
}

move_t* gen(move_t* moves) {
	moves = gen_kings(moves);
	moves = gen_pawns(moves);
	moves = gen_knights(moves);
	moves = gen_bishops(moves);
	moves = gen_rooks(moves);
	moves = gen_queens(moves);
	moves = gen_ep(moves);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

bool check() {
	piece_t piece = (Piece_King | (color ^ Piece_Color)) & Piece_Index;
	return check_to(get_piece(piece).square);
}

void set_init(piece_square_t ps) {
	set_square(ps);
	set_piece(ps);
}

void clear(piece_square_t ps) {
	clear_square(ps);
	clear_piece(ps);
}

void set(piece_square_t ps) {
	set_square(ps);
	set_piece(ps);
}

void set_prim_to(piece_square_t to) {
	to.piece |= Piece_Moved;
	set_square(to);
	set_piece(to);
}

void clear_ep() {
	state.piecemask &= ~(1ull << Piece_EP);
}

void move_make(move_t move) {
	clear_ep();

	clear(move.sec.from);
	set(move.sec.to);
	clear(move.prim.from);
	set_prim_to(move.prim.to);

	color ^= Piece_Color;

	state.ep.square = move.sec.to.square;
}

void move_unmake(move_t move) {
	color ^= Piece_Color;

	clear(move.prim.to);
	set(move.prim.from);
	clear(move.sec.to);
	set(move.sec.from);
}

uint64_t perft(move_t* moves, uint8_t depth, uint8_t div, char* buffer, char* str);
char* move_write(char* str, move_t move);

uint64_t perft_divide(move_t* moves, move_t move, uint8_t depth, uint8_t div, char* buffer, char* str) {
	str = move_write(str, move);
	*str++ = ' ';
	uint64_t count = perft(moves, depth, div, buffer, str);
	*str = 0;
	printf("%s%11" PRIu64 "\n", buffer, count);
	return count;
}

uint64_t perft(move_t* moves, uint8_t depth, uint8_t div, char* buffer, char* str) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	state_t state2;
	if (!depth)
		return 1;
	pEnd = gen(moves);
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		state2 = state;
		move_make(*pCurr);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check())
			count += !div
				? perft(pEnd, depth - 1, div, buffer, str)
				: perft_divide(pEnd, *pCurr, depth - 1, div - 1, buffer, str);
		move_unmake(*pCurr);
		state = state2;
	}
	return count;
}

char get_piece_char(piece_t piece) {
	return piece_chars[(piece & Piece_Index) >> Shift_Type];
}

uint8_t find_char(char c, const char chars[], uint8_t count) {
	uint8_t i;
	for (i = 0; i < count && c != chars[i]; ++i);
	return i;
}

uint8_t find_type4(char c) {
	return find_char(c, piece_chars, Count_Type4);
}

uint8_t find_color(char c) {
	return find_char(c, color_chars, Count_Colors);
}

uint8_t find_castling(char c) {
	return find_char(c, castling_chars, Castling_Count);
}

uint8_t get_moved(uint8_t type4, square_t square) {
	return (square & Square_Rank) == piece_ranks[type4]
		? 0
		: Piece_Moved;
}

const char* fen_read_error(char c) {
	fprintf(stderr, "Unexpected character %c\n", c);
	return 0;
}

const char* fen_read_char(const char* str, char e) {
	char c = *str++;
	return c == e
		? str
		: fen_read_error(c);
}

const char* fen_read_piece_clear(const char* str, piece_square_t* ps) {
	char c = *str++;
	for (uint8_t i = 0; i < c - Char_Zero; ++i, ++ps->square) {
		clear_square(*ps);
	}
	return !(ps->square & Square_File) || !(ps->square & Square_FileInvalid)
		? str
		: fen_read_error(c);
}

char* fen_write_piece_clear(char* str, uint8_t* count) {
	if (*count) {
		*str++ = *count + Char_Zero;
		*count = 0;
	}
	return str;
}

const char* fen_read_piece_set(const char* str, piece_square_t* ps) {
	char c = *str++;
	uint8_t type4;
	if (!((type4 = find_type4(c)) & (Type_Count - 1))) {
		return fen_read_error(c);
	}
	ps->piece = (type4 << Shift_Type) | get_moved(type4, ps->square);
	if (!(ps->piece & Piece_Black)) {
		ps->piece |= Piece_White;
	}
	set_square(*ps);
	++ps->square;
	return str;
}

char* fen_write_piece_set(char* str, piece_t piece, uint8_t* count) {
	str = fen_write_piece_clear(str, count);
	*str++ = get_piece_char(piece);
	return str;
}

const char* fen_read_piece(const char* str, piece_square_t* ps) {
	char c;
	return ((c = *str) > Char_Zero && c <= Char_Zero + Count_Files)
		? fen_read_piece_clear(str, ps)
		: fen_read_piece_set(str, ps);
}

char* fen_write_piece(char* str, square_t square, uint8_t* count) {
	piece_t piece;
	if ((piece = get_square(square))) {
		str = fen_write_piece_set(str, piece, count);
	}
	else {
		++*count;
	}
	return str;
}

const char* fen_read_file(const char* str, square_t* square) {
	char c;
	if ((c = *str++) < Char_File || c >= Char_File + Count_Files) {
		return fen_read_error(c);
	}
	*square |= (c - Char_File);
	return str;
}

char* file_write(char* str, square_t square) {
	*str++ = (square & Square_File) + Char_File;
	return str;
}

const char* fen_read_rank(const char* str, square_t* square) {
	char c;
	if ((c = *str++) < Char_Rank || c >= Char_Rank + Count_Ranks) {
		return fen_read_error(c);
	}
	*square |= (c - Char_Rank) << Shift_Rank;
	return str;
}

char* rank_write(char* str, square_t square) {
	*str++ = (square >> Shift_Rank) + Char_Rank;
	return str;
}

const char* fen_read_square(const char* str, square_t* square) {
	*square = 0;
	return (str = fen_read_file(str, square)) && (str = fen_read_rank(str, square))
		? str
		: 0;
}

char* square_write(char* str, square_t square) {
	str = file_write(str, square);
	str = rank_write(str, square);
	return str;
}

const char* fen_read_squares(const char* str) {
	piece_square_t ps;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		for (ps.square = rank << Shift_Rank; !(ps.square & Square_FileInvalid); ) {
			if (!(str = fen_read_piece(str, &ps))) {
				return 0;
			}
		}
		if (rank && !(str = fen_read_char(str, '/'))) {
			return 0;
		}
	}
	return str;
}

char* fen_write_squares(char* str) {
	square_t square;
	uint8_t count = 0;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		for (square = rank << Shift_Rank; !(square & Square_FileInvalid); ++square) {
			str = fen_write_piece(str, square, &count);
		}
		str = fen_write_piece_clear(str, &count);
		if (rank) {
			*str++ = '/';
		}
	}
	return str;
}

const char* fen_read_color(const char* str) {
	char c = *str++;
	uint8_t i;
	if ((i = find_color(c)) == Count_Colors) {
		return fen_read_error(c);
	}
	color = color_values[i];
	state.ep.square = color_ranks[i] | Square_FileInvalid;
	return str;
}

char* fen_write_color(char* str) {
	*str++ = color_chars[color == Piece_Black];
	return str;
}

const char* fen_read_castling(const char* str) {
	char c = *str++;
	uint8_t i;
	piece_square_t ps;
	if ((i = find_castling(c)) == Castling_Count
		|| (ps.piece = get_square(ps.square = castling_rooks[i]))
			!= (Piece_Rook | Piece_Moved | color_values[i >> Shift_Castling])) {
				return fen_read_error(c);
	}
	ps.piece &= ~Piece_Moved;
	set_square(ps);
	return str;
}

char* fen_write_castling(char* str, uint8_t i) {
	piece_t color = color_values[i >> Shift_Castling];
	piece_t rook = Piece_Rook | color | (i & Piece_Castling);
	piece_t king = Piece_King | color;
	if (has_piece(rook)
		&& !(get_piece(rook & Piece_Index).piece & Piece_Moved)
		&& !(get_piece(king & Piece_Index).piece & Piece_Moved)) {
			*str++ = castling_chars[i];
	}
	return str;
}

const char* fen_read_castling_chars(const char* str) {
	do {
		if (!(str = fen_read_castling(str))) {
			return 0;
		}
	} while (*str && *str != ' ');
	return str;
}

char* fen_write_castling_chars(char* str) {
	str = fen_write_castling(str, Castling_White | Rook_H);
	str = fen_write_castling(str, Castling_White | Rook_A);
	str = fen_write_castling(str, Castling_Black | Rook_H);
	str = fen_write_castling(str, Castling_Black | Rook_A);
	return str;
}

const char* fen_read_ep_square(const char* str) {
	state.piecemask |= (1ull << Piece_EP);
	return fen_read_square(str, &state.ep.square);
}

char* fen_write_ep_square(char* str) {
	return square_write(str, state.ep.square);
}

const char* fen_read_castlings(const char* str) {
	return *str != '-'
		? fen_read_castling_chars(str)
		: ++str;
}

char* fen_write_castlings(char* str) {
	char* start = str;
	if ((str = fen_write_castling_chars(str)) == start) {
		*str++ = '-';
	}
	return str;
}

const char* fen_read_ep(const char* str) {
	return *str != '-'
		? fen_read_ep_square(str)
		: ++str;
}

char* fen_write_ep(char* str) {
	if (!(state.piecemask & (1ull << Piece_EP))) {
		*str++ = '-';
	} else {
		str = fen_write_ep_square(str);
	}
	return str;
}

const char* fen_read(const char* str) {
	state.piecemask = 0;
	if (!(str = fen_read_squares(str))
		|| !(str = fen_read_char(str, ' ')) || !(str = fen_read_color(str))
		|| ((*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_castlings(str))
		|| (*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_ep(str)))))))) {
		return 0;
	}
	return str;
}

char* fen_write(char* str) {
	str = fen_write_squares(str);
	*str++ = ' ';
	str = fen_write_color(str);
	*str++ = ' ';
	str = fen_write_castlings(str);
	*str++ = ' ';
	str = fen_write_ep(str);
	return str;
}

bool set_piece_unmoved(piece_square_t ps) {
	piece_square_t ps2 = find_index(ps);
	if (!ps2.value) {
		fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
		return false;
	}
	set_init(ps2);
	return true;
}

bool set_piece_moved(piece_square_t ps) {
	piece_square_t ps2 = find_index_moved(ps);
	if (!ps2.value) {
		fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
		return false;
	}
	set_init(ps2);
	return true;
}

bool set_pieces_unmoved() {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(ps.square)) && !(ps.piece & Piece_Moved)
				&& !set_piece_unmoved(ps)) {
					return false;
			}
		}
	}
	return true;
}

bool set_pieces_moved() {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if (((ps.piece = get_square(ps.square)) & Piece_Moved)
				&& !set_piece_moved(ps)) {
					return false;
			}
		}
	}
	return true;
}

bool set_pieces_ep_get(piece_square_t* ep_pawn) {
	ep_pawn->square = state.ep.square ^ Square_Rank2;
	square_t from_square = ep_pawn->square ^ Square_Rank3;
	if (((state.ep.square & Square_Rank) != color_ranks[color == Piece_Black]
		|| get_square(from_square)
		|| get_square(state.ep.square))
		|| (ep_pawn->piece = get_square(ep_pawn->square)) != (Piece_Pawn0 | Piece_Moved | (color ^ Piece_Color))) {
			fprintf(stderr, "Invalid e.p. square.\n");
			return false;
	}
	return true;
}

bool set_pieces_ep_unmoved(piece_square_t* ep_pawn) {
	if (state.piecemask & (1ull << Piece_EP)) {
		if (!set_pieces_ep_get(ep_pawn)) {
			return false;
		}
		ep_pawn->piece &= ~Piece_Moved;
		set_square(*ep_pawn);
	}
	return true;
}

bool set_pieces_ep_moved(piece_square_t* ep_pawn) {
	if (state.piecemask & (1ull << Piece_EP)) {
		ep_pawn->piece = get_square(ep_pawn->square) | Piece_Moved;
		set_init(*ep_pawn);
	}
	return true;
}

bool validate_kings() {
	for (uint8_t i = 0; i < Count_Colors; ++i) {
		piece_t piece = Piece_King | color_values[i];
		if (!has_piece(piece)) {
			fprintf(stderr, "Missing %c.\n", get_piece_char(piece));
			return false;
		}
	}
	return true;
}

bool validate_check() {
	if (check()) {
		fprintf(stderr, "Illegal position.\n");
		return false;
	}
	return true;
}

bool validate() {
	return validate_kings()
		&& validate_check();
}

bool set_pieces() {
	piece_square_t ep_pawn;
	return set_pieces_ep_unmoved(&ep_pawn)
		&& set_pieces_unmoved()
		&& set_pieces_moved()
		&& set_pieces_ep_moved(&ep_pawn)
		&& validate();
}

char* board_write(char* str) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << Shift_Rank;
		str = rank_write(str, square);
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = get_square(square);
			*str++ = ' ';
			*str++ = piece
				? get_piece_char(piece)
				: '.';
		}
		*str++ = '\n';
	}
	*str = 0;
	return str;
}

char* move_write(char* str, move_t move) {
	str = square_write(str, move.prim.from.square);
	str = square_write(str, move.prim.to.square);
	if (move.prim.from.piece != move.prim.to.piece) {
		*str++ = get_piece_char(move.prim.to.piece);
	}
	*str = 0;
	return str;
}

const char* read_uint8(const char* str, uint8_t* result) {
	unsigned int i = 0;
	char c;
	while ((c = *str++)) {
		if (c < Char_Zero || c > Char_Nine) {
			return 0;
		}
		i = i * 10 + c - Char_Zero;
	}
	*result = i;
	return i <= UINT8_MAX
		? str
		: 0;
}

const char* read_uint64(const char* str, uint64_t* result) {
	char c;
	*result = 0;
	while ((c = *str++)) {
		if (c < Char_Zero || c > Char_Nine) {
			return 0;
		}
		*result = *result * 10 + c - Char_Zero;
	}
	return str;
}

bool args_read_max(const char* arg, params_t* params) {
	return read_uint8(arg, &params->max) && params->max;
}

bool args_read_div(const char* arg, params_t* params) {
	return read_uint8(arg, &params->div) && params->div;
}

bool args_read_result(const char* arg, params_t* params) {
	return read_uint64(arg, &params->result);
}

bool args_read_flag(const char* arg, params_t* params) {
	switch (arg[1]) {
	case 'd':
		return args_read_div(&arg[2], params);
	case 'l':
		params->min = 0;
		return true;
	default:
		return false;
	}
}

bool args_read(int argc, const char* argv[], params_t* params) {
	params->min = 0;
	params->max = UINT8_MAX;
	params->div = 0;
	params->fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
	params->result = UINT64_MAX;
	
	for (uint8_t i = 1; i < argc; ++i) {
		if (argv[i][0] == '-') {
			if (!args_read_flag(argv[i], params)) {
				return false;
			}
		} else if (params->max == UINT8_MAX) {
			if (!args_read_max(argv[i], params)) {
				params->fen = argv[i];
			} else {
				params->min = params->max;
			}
		} else if (!args_read_result(argv[i], params)) {
			return false;
		}
	}
	return true;
}

int main(int argc, const char* argv[]) {
	static move_t moves[Max_Moves];
	static char buffer[Max_Chars];

	params_t params;
	uint64_t count = 0;

	if (!args_read(argc, argv, &params)) {
		printf("Usage: perft [<fen>] [<depth> [<result>]] [-d<divide>] [-l]\n");
		return -1;
	}

	if (!fen_read(params.fen)
		|| !set_pieces()) {
			return 1;
	}

	if (params.result == UINT64_MAX) {
		board_write(buffer);
		printf("%s\n", buffer);
	} else {
		fen_write(buffer);
		printf("\nPosition: %s\n", buffer);
	}

	for (uint8_t depth = params.min; depth <= params.max; ++depth) {
		count = perft(moves, depth, params.div, buffer, buffer);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return params.result == UINT64_MAX || count == params.result
		? 0
		: 2;
}
