#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
#define sscanf sscanf_s
#endif

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
	Shift_Odd         = 1,
	Shift_Type        = 2,
	Shift_Rank        = 4,
};

enum Piece {
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
	Piece_White    = 0x40,
	Piece_Color    = Piece_Black | Piece_White,

	Piece_Index    = Piece_Index2 | Piece_Type | Piece_Black,

	Piece_EP       = Piece_Black,

	Piece_Moved    = 0x80
};

enum Square {
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
	Count_Ranks    =   8,
	Count_Files    =   8,
	Count_Squares  = 128,

	Count_Pawns    =   8,
	Count_Knights  =   4,
	Count_Knights2 =   2,
	Count_Bishops  =   4,
	Count_Bishops2 =   2,
	Count_Rooks    =   4,
	Count_Queens   =   4,
	Count_Pieces   =  64,
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
	struct {
		piece_square_t from;
		piece_square_t to;
	} prim;
	struct {
		piece_square_t from;
		piece_square_t to;
	} sec;
} move_t;

typedef struct {
	uint64_t piecemask;
	square_t ep;
} state_t;

piece_t squares[Count_Squares];
piece_square_t pieces[Count_Pieces];
uint64_t piecemask;
piece_t color;

char piece_chars[] = ":KPPNBRQ;kppnbrq";

void board_init() {
	memset(squares, 0, Count_Squares);

	squares[0x00] = Piece_Rook   + Piece_White;
	squares[0x01] = Piece_Knight + Piece_White + Piece_Moved;
	squares[0x02] = Piece_Bishop + Piece_White + Piece_Moved;
	squares[0x03] = Piece_Queen  + Piece_White + Piece_Moved;
	squares[0x04] = Piece_King   + Piece_White;
	squares[0x05] = Piece_Bishop + Piece_White + Piece_Moved;
	squares[0x06] = Piece_Knight + Piece_White + Piece_Moved;
	squares[0x07] = Piece_Rook   + Piece_White;

	for (uint8_t i = 0; i < Count_Files; ++i) {
		squares[i + 0x10] = Piece_Pawn0 + Piece_White;
		squares[i + 0x60] = Piece_Pawn0 + Piece_Black;
	}

	squares[0x70] = Piece_Rook   + Piece_Black;
	squares[0x71] = Piece_Knight + Piece_Black + Piece_Moved;
	squares[0x72] = Piece_Bishop + Piece_Black + Piece_Moved;
	squares[0x73] = Piece_Queen  + Piece_Black + Piece_Moved;
	squares[0x74] = Piece_King   + Piece_Black;
	squares[0x75] = Piece_Bishop + Piece_Black + Piece_Moved;
	squares[0x76] = Piece_Knight + Piece_Black + Piece_Moved;
	squares[0x77] = Piece_Rook   + Piece_Black;

	color = Piece_White;
	piecemask = 0;
}

uint8_t get_index2_knight(square_t square) {
	return (((square ^ (square >> Shift_Rank)) & 1) ^ 1) * 3;
}

uint8_t get_index2(square_t square) {
	return ((square ^ (square >> Shift_Rank)) & 1) << Shift_Odd;
}

piece_square_t find_index_to(piece_square_t ps) {
	for (uint64_t mask = piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

piece_square_t find_index_to_knight(piece_square_t ps) {
	ps.value += get_index2_knight(ps.square);
	return ps;
}

piece_square_t find_index_to_bishop(piece_square_t ps) {
	ps.value += get_index2(ps.square);
	return ps;
}

piece_square_t find_index_error(piece_square_t ps) {
	ps.value = 0;
	return ps;
}

piece_square_t find_index_moved_king(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return (ps2.value & Piece_Index2)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_king(piece_square_t ps) {
	if ((ps.square & Square_Rank) != (!(ps.piece & Piece_Black) ? Square_Rank1 : Square_Rank8)) {
		ps.piece |= Piece_Moved;
		return ps;
	}
	return find_index_moved_king(ps);
}

piece_square_t find_index_unmoved_pawn(piece_square_t ps) {
	ps.piece |= (ps.square & Square_File);
	return (piecemask & (1ull << (ps.value & Piece_Index)))
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_pawn(piece_square_t ps) {
	if ((ps.square & Square_Rank) != (!(ps.piece & Piece_Black) ? Square_Rank2 : Square_Rank7)) {
		ps.piece |= Piece_Moved;
		return ps;
	}
	return find_index_unmoved_pawn(ps);
}

piece_square_t find_index_knight(piece_square_t ps) {
	piece_square_t ps2 = find_index_to_knight(ps);
	return (piecemask & (1ull << (ps2.value & Piece_Index)))
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_bishop(piece_square_t ps) {
	ps = find_index_to_bishop(ps);
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & (Piece_Type | Piece_Odd))
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_rook(piece_square_t ps) {
	ps.piece |= (piecemask >> (Piece_King | (ps.piece & Piece_Black))) & 1;
	return (piecemask & (1ull << (ps.value & Piece_Index)))
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_queen(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & Piece_Type)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_moved_pawn(piece_square_t ps) {
	piece_square_t ps2 = find_index_to(ps);
	return ((ps.value ^ ps2.value) & Piece_TypePawn)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_moved_rook(piece_square_t ps) {
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
		return find_index_moved_king(ps);
	case Piece_Pawn0:
		return find_index_moved_pawn(ps);
	case Piece_Knight:
		return find_index_knight(ps);
	case Piece_Bishop:
		return find_index_bishop(ps);
	case Piece_Rook:
		return find_index_moved_rook(ps);
	case Piece_Queen:
		return find_index_queen(ps);
	default:
		return find_index_error(ps);
	}
}

move_t* gen_null(move_t* moves) {
	piece_square_t nullps = { 0x0800 };
	move_t move = {
		.prim = {
			.from = nullps,
			.to = nullps
		},
		.sec = {
			.from = nullps,
			.to = nullps
		}
	};
	*moves++ = move;
	return moves;
}

move_t* gen_promo_knight(move_t* moves, move_t move, piece_square_t to) {
	to.piece = Piece_Knight | color | Piece_Moved;
	move.prim.to = find_index_to_knight(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_bishop(move_t* moves, move_t move, piece_square_t to) {
	to.piece = Piece_Bishop | color | Piece_Moved;
	move.prim.to = find_index_to_bishop(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_rook(move_t* moves, move_t move, piece_square_t to) {
	to.piece = Piece_Rook | color | Piece_Moved;
	move.prim.to = find_index_to(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_queen(move_t* moves, move_t move, piece_square_t to) {
	to.piece = Piece_Queen | color | Piece_Moved;
	move.prim.to = find_index_to(to);
	*moves++ = move;
	return moves;
}

move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to, uint8_t promo) {
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo_knight(moves, move, to);
		moves = gen_promo_bishop(moves, move, to);
		moves = gen_promo_rook(moves, move, to);
		moves = gen_promo_queen(moves, move, to);
		return moves;
	}
	*moves++ = move;
	return moves;
}

move_t* gen_push_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!(from2.piece = squares[from2.square = to.square += vector])) {
		move_t move = {
			.prim = {
				.from = from,
				.to = to
			},
			.sec = {
				.from = from2,
				.to = { 0x0800 }
			}
		};
		moves = gen_promo_pawn(moves, move, to, promo);
		if (!(from.piece & Piece_Moved)
			&& !squares[to.square += vector]) {
				move.prim.to = to;
				move.sec.to.value = from2.value | Piece_EP | 0x0800;
				*moves++ = move;
		}
	}
	return moves;
}

move_t* gen_vector_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& (from2.piece = squares[from2.square = to.square]) & (color ^ Piece_Color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { 0x0800 }
				}
			};
			moves = gen_promo_pawn(moves, move, to, promo);
	}
	return moves;
}

bool check_vector_pawn(square_t square, vector_t vector) {
	return !((square += vector) & Square_Invalid)
		&& (squares[square] & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color);
}

move_t* gen_vector_ep(move_t* moves, vector_t vector) {
	piece_square_t to = {
		.square = pieces[Piece_EP].square & ~Square_FileInvalid
	};
	piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = squares[from.square]) & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color)) {
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
					.to = { 0x0800 }
				}
		};
		*moves++ = move;
	}
	return moves;
}

move_t* gen_vector_king(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { 0x0800 }
				}
			};
			*moves++ = move;
	}
	return moves;
}

move_t* gen_vector_knight(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = { to.value ^ Piece_Odd }
				},
				.sec = {
					.from = from2,
					.to = { 0x0800 }
				}
			};
			*moves++ = move;
	}
	return moves;
}

move_t* gen_vector_slider(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2 = {0};
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { 0x0800 }
				}
			};
			*moves++ = move;
	}
	return moves;
}

bool check_vector(square_t src, square_t dest, vector_t vector) {
	do {
		src += vector;
	} while (!squares[src]);
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
	return piecemask & (1ull << Piece_EP)
		? color == Piece_White
			? gen_ep_white(moves)
			: gen_ep_black(moves)
		: moves;
}

move_t* gen_king(move_t* moves, piece_square_t from) {
	for (uint8_t i = 0; i < 8; ++i) {
		moves = gen_vector_king(moves, from, vectors[i]);
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
		moves = gen_vector_knight(moves, from, vectors[i]);
	}
	return moves;
}

bool check_knight(piece_square_t from, square_t dest) {
	square_t src = from.square;
	uint8_t delta = abs(dest - src);
	return delta == Vec_NWW || delta == Vec_NEE || delta == Vec_NNW || delta == Vec_NNE;
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
	return gen_king(moves, pieces[piece]);
}

bool check_kings(square_t dest) {
	piece_t piece = (Piece_King | color) & Piece_Index;
	return check_king(pieces[piece], dest);
}

move_t* gen_pawns(move_t* moves) {
	piece_t piece = (Piece_Pawn0 | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Pawns; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_pawn(moves, pieces[piece]);
		}
	}
	return moves;
}

move_t* gen_knights(move_t* moves) {
	piece_t piece = (Piece_Knight | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Knights; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_knight(moves, pieces[piece]);
		}
	}
	return moves;
}

bool check_knights(square_t dest) {
	piece_t piece = ((Piece_Knight + get_index2(dest)) | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Knights2; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_knight(pieces[piece], dest)) {
			return true;
		}
	}
	return false;
}

move_t* gen_bishops(move_t* moves) {
	piece_t piece = (Piece_Bishop | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Bishops; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_bishop(moves, pieces[piece]);
		}
	}
	return moves;
}

bool check_bishops(square_t dest) {
	piece_t piece = ((Piece_Bishop + get_index2(dest)) | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Bishops2; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_bishop(pieces[piece], dest)) {
			return true;
		}
	}
	return false;
}

move_t* gen_rooks(move_t* moves) {
	piece_t piece = (Piece_Rook | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_rook(moves, pieces[piece]);
		}
	}
	return moves;
}

bool check_rooks(square_t dest) {
	piece_t piece = (Piece_Rook | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_rook(pieces[piece], dest)) {
			return true;
		}
	}
	return false;
}

move_t* gen_queens(move_t* moves) {
	piece_t piece = (Piece_Queen | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_queen(moves, pieces[piece]);
		}
	}
	return moves;
}

bool check_queens(square_t dest) {
	piece_t piece = (Piece_Queen | color) & Piece_Index;
	uint64_t mask = piecemask >> piece;
	for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_queen(pieces[piece], dest)) {
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
	moves = gen_null(moves);
#endif
	return moves;
}

bool check() {
	piece_t piece = (Piece_King | (color ^ Piece_Color)) & Piece_Index;
	return check_to(pieces[piece].square);
}

void clear_square(piece_square_t ps) {
	squares[ps.square] = 0x00;
}

void set_square(piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

void clear_piece(piece_square_t ps) {
	piece_t piece = ps.piece & Piece_Index;
	piecemask &= ~(1ull << piece);
}

void set_piece(piece_square_t ps) {
	piece_t piece = ps.piece & Piece_Index;
	pieces[piece] = ps;
	piecemask |= (1ull << piece);
}

void clear_prim_from(piece_square_t from) {
	clear_square(from);
	clear_piece(from);
}

void set_prim_from(piece_square_t from) {
	set_square(from);
	set_piece(from);
}

void clear_prim_to(piece_square_t to) {
	clear_square(to);
	clear_piece(to);
}

void set_prim_to(piece_square_t to) {
	to.piece |= Piece_Moved;
	set_square(to);
	set_piece(to);
}

void clear_sec(piece_square_t ps) {
	clear_square(ps);
	clear_piece(ps);
}

void set_sec(piece_square_t ps) {
	set_square(ps);
	set_piece(ps);
}

void clear_ep() {
	piecemask &= ~(1ull << Piece_EP);
}

square_t move_make(move_t move) {
	clear_ep();

	clear_sec(move.sec.from);
	set_sec(move.sec.to);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	color ^= Piece_Color;

	return move.sec.to.square;
}

void move_unmake(move_t move) {
	color ^= Piece_Color;

	clear_prim_to(move.prim.to);
	set_prim_from(move.prim.from);
	clear_sec(move.sec.to);
	set_sec(move.sec.from);
}

uint64_t perft(move_t* moves, uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	if (!depth)
		return 1;
	pEnd = gen(moves);
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		state_t state = {
			.piecemask = piecemask,
			.ep = move_make(*pCurr)
		};
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check())
			count += perft(pEnd, depth - 1);
		move_unmake(*pCurr);
		piecemask = state.piecemask;
		pieces[Piece_EP].square = state.ep;
	}
	return count;
}

char get_piece_char(piece_t piece) {
	return piece_chars[(piece & Piece_Index) >> Shift_Type];
}

bool set_pieces_unmoved() {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = squares[ps.square]) && !(ps.piece & Piece_Moved)) {
				if (!(ps2 = find_index(ps)).value) {
					fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
					return false;
				}
				set_square(ps2);
				set_piece(ps2);
			}
		}
	}
	return true;
}

bool set_pieces_moved() {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = squares[ps.square]) & Piece_Moved) {
				if (!(ps2 = find_index_moved(ps)).value) {
					fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
					return false;
				}
				set_square(ps2);
				set_piece(ps2);
			}
		}
	}
	return true;
}

bool set_pieces() {
	return set_pieces_unmoved()
		&& set_pieces_moved();
}

char* board_write(char* str) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << Shift_Rank;
		*str++ = rank + '1';
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = squares[square];
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

char buffer[1024];
move_t moves[1024];

int main(int argc, const char* argv[]) {
	uint8_t max = 255;

	if (argc > 1
		&& (!sscanf(argv[1], "%hhu", &max) || !max)) {
			printf("Usage: perft <depth>\n");
			return -1;
	}

	board_init();
	if (!set_pieces()) {
		return 1;
	}

	board_write(buffer);
	printf("%s\n", buffer);

	for (uint8_t depth = 0; depth <= max; ++depth) {
		uint64_t count = perft(moves, depth);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return 0;
}
