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
	Shift_Type = 2,
	Shift_Rank = 4
};

enum Piece {
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

enum TypeMask {
	TypeMask_King   = 1 << Piece_King,
	TypeMask_Pawn   = (1 << Piece_Pawn0) | (1 << Piece_Pawn1),
	TypeMask_Knight = 1 << Piece_Knight,
	TypeMask_Bishop = 1 << Piece_Bishop,
	TypeMask_Rook   = 1 << Piece_Rook,
	TypeMask_Queen  = 1 << Piece_Queen
};

enum Square {
	Square_File        = 0x07,
	Square_FileInvalid = 0x08,

	Square_Rank1       = 0x00,
	Square_Rank2       = 0x10,
	Square_Rank3       = 0x20,
	Square_Rank6       = 0x50,
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

enum Dir {
	Dir_SW,
	Dir_S,
	Dir_SE,
	Dir_W,
	Dir_E,
	Dir_NW,
	Dir_N,
	Dir_NE
};

enum DirMask {
	DirMask_SW = 1 << Dir_SW,
	DirMask_S  = 1 << Dir_S,
	DirMask_SE = 1 << Dir_SE,
	DirMask_W  = 1 << Dir_W,
	DirMask_E  = 1 << Dir_E,
	DirMask_NW = 1 << Dir_NW,
	DirMask_N  = 1 << Dir_N,
	DirMask_NE = 1 << Dir_NE
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

typedef uint32_t type_mask_t;
typedef uint8_t  dir_mask_t;

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
	squares[0x05] = Piece_Bishop + Piece_White + Piece_Moved + 2;
	squares[0x06] = Piece_Knight + Piece_White + Piece_Moved + 3;
	squares[0x07] = Piece_Rook   + Piece_White + 1;

	for (uint8_t i = 0; i < Count_Files; ++i) {
		squares[i + 0x10] = Piece_Pawn0 + Piece_White + i;
		squares[i + 0x60] = Piece_Pawn0 + Piece_Black + i;
	}

	squares[0x70] = Piece_Rook   + Piece_Black;
	squares[0x71] = Piece_Knight + Piece_Black + Piece_Moved + 3;
	squares[0x72] = Piece_Bishop + Piece_Black + Piece_Moved + 2;
	squares[0x73] = Piece_Queen  + Piece_Black + Piece_Moved;
	squares[0x74] = Piece_King   + Piece_Black;
	squares[0x75] = Piece_Bishop + Piece_Black + Piece_Moved;
	squares[0x76] = Piece_Knight + Piece_Black + Piece_Moved;
	squares[0x77] = Piece_Rook   + Piece_Black + 1;

	color = Piece_White;
	piecemask = 0;
}

static inline
uint8_t get_index(register square_t square) {
	return (((square ^ (square >> Shift_Rank)) & 1) << 1);
}

static inline
move_t* gen_promo_pawn(move_t* moves, register move_t move, register piece_square_t to, uint8_t promo, uint8_t color) {
	if ((to.square & Square_Rank) == promo) {
		move.prim.to.piece = Piece_Queen | color | Piece_Moved;
	}
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_push_pawn(move_t* moves, register piece_square_t from, vector_t vector, uint8_t promo, uint8_t color, uint8_t color2) {
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!(from2.piece = squares[from2.square = to.square += vector])) {
		register move_t move = {
			.prim = {
				.from = from,
				.to = to
			},
			.sec = {
				.from = from2,
				.to = { 0x0800 }
			}
		};
		moves = gen_promo_pawn(moves, move, to, promo, color);
		if (!(from.piece & Piece_Moved)
			&& !squares[to.square += vector]) {
				move.prim.to = to;
				move.sec.to.value = from2.value | Piece_EP | 0x0800;
				*moves++ = move;
		}
	}
	return moves;
}

static inline
move_t* gen_vector_pawn(move_t* moves, register piece_square_t from, vector_t vector, uint8_t promo, uint8_t color, uint8_t color2) {
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& (from2.piece = squares[from2.square = to.square]) & color2) {
			register move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { 0x0800 }
				}
			};
			moves = gen_promo_pawn(moves, move, to, promo, color);
	}
	return moves;
}

static inline
move_t* gen_vector_ep(move_t* moves, vector_t vector, uint8_t color, uint8_t color2) {
	register piece_square_t to = {
		.square = pieces[Piece_EP].square & ~Square_FileInvalid
	};
	register piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = squares[from.square]) & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color)) {
			register move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = {
						.piece = (to.square & Square_File) | Piece_Pawn0 | (color2) | Piece_Moved,
						.square = to.square ^ Square_Rank2
					},
					.to = { 0x0800 }
				}
		};
		*moves++ = move;
	}
	return moves;
}

static inline
move_t* gen_vector_king(move_t* moves, register piece_square_t from, vector_t vector, uint8_t color) {
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			register move_t move = {
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

static inline
move_t* gen_vector_knight(move_t* moves, register piece_square_t from, vector_t vector, uint8_t color) {
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			register move_t move = {
				.prim = {
					.from = from,
					.to = { to.value ^ 0x02 }
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

bool check_square_neighbor(register square_t square, type_mask_t type_mask, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	register piece_t piece = squares[square];
	if (piece & color) {
		return type_mask & (1 << (piece & Piece_Type));
	}
	if (!piece) {
		*dir_mask |= (1 << dir);
	}
	return 0;
}

static inline
bool check_square_knight(register square_t square, uint8_t color) {
	register piece_t piece = squares[square];
	return (piece & (Piece_Type | Piece_Color)) == (Piece_Knight | color);
}

static inline
bool check_vector(register square_t square, type_mask_t type_mask, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return !((square += vector) & Square_Invalid)
		&& check_square_neighbor(square, type_mask, dir, dir_mask, color);
}

static inline
bool check_vector_knight(register square_t square, vector_t vector, uint8_t color) {
	return !((square += vector) & Square_Invalid)
		&& check_square_knight(square, color);
}

static inline
bool check_vector_pawn(register square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King | TypeMask_Pawn,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_diag(register square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_ortho(register square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Rook | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
move_t* gen_vector_slider(move_t* moves, register piece_square_t from, vector_t vector, uint8_t color) {
	register piece_square_t to = from;
	register piece_square_t from2 = {0};
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			register move_t move = {
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

static inline
bool check_vector_slider(register square_t src, register square_t dest, vector_t vector, uint8_t dir, dir_mask_t dir_mask) {
	if (!(dir_mask & (1 << dir))) {
		return false;
	}
	for (src += vector; !squares[src += vector]; );
	return src == dest;
}

static inline
bool check_vert(register square_t src, register square_t dest, register int8_t drank, dir_mask_t dir_mask) {
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_N, Dir_N, dir_mask)
		: check_vector_slider(src, dest, Vec_S, Dir_S, dir_mask);
}

static inline
bool check_horiz(register square_t src, register square_t dest, register int8_t dfile, dir_mask_t dir_mask) {
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_E, Dir_E, dir_mask)
		: check_vector_slider(src, dest, Vec_W, Dir_W, dir_mask);
}

static inline
bool check_diag1(register square_t src, register square_t dest, register int8_t drank, dir_mask_t dir_mask) {
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_NE, Dir_NE, dir_mask)
		: check_vector_slider(src, dest, Vec_SW, Dir_SW, dir_mask);
}

static inline
bool check_diag2(register square_t src, register square_t dest, register int8_t dfile, dir_mask_t dir_mask) {
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_SE, Dir_SE, dir_mask)
		: check_vector_slider(src, dest, Vec_NW, Dir_NW, dir_mask);
}

static inline
bool check_ortho(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank, dir_mask_t dir_mask) {
	return !dfile
		? check_vert(src, dest, drank, dir_mask)
		: !drank
			? check_horiz(src, dest, dfile, dir_mask)
			: false;
}

static inline
bool check_diag(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank, dir_mask_t dir_mask) {
	return dfile == drank
		? check_diag1(src, dest, drank, dir_mask)
		: dfile == -drank
			? check_diag2(src, dest, dfile, dir_mask)
			: false;
}

static inline
move_t* gen_pawn_white(move_t* moves, register piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_NW, Square_Rank8, Piece_White, Piece_Black);
	moves = gen_vector_pawn(moves, from, Vec_NE, Square_Rank8, Piece_White, Piece_Black);
	return gen_push_pawn(moves, from, Vec_N, Square_Rank8, Piece_White, Piece_Black);
}

static inline
move_t* gen_ep_white(move_t* moves) {
	if (piecemask & (1ull << Piece_EP)) {
		moves = gen_vector_ep(moves, Vec_SW, Piece_White, Piece_Black);
		moves = gen_vector_ep(moves, Vec_SE, Piece_White, Piece_Black);
	}
	return moves;
}

static inline
bool check_neighbors_white_s(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_pawn(square, Vec_SW, Dir_SW, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_White)
		|| check_vector_pawn (square, Vec_SE, Dir_SE, dir_mask, Piece_White));
}

static inline
bool check_neighbors_white_we(register square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_White);
}

static inline
bool check_neighbors_white_n(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_diag(square, Vec_NW, Dir_NW, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_N,  Dir_N,  dir_mask, Piece_White)
		|| check_vector_diag (square, Vec_NE, Dir_NE, dir_mask, Piece_White));
}

static inline
bool check_neighbors_white(register square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_white_s(square, dir_mask)
		|| check_neighbors_white_we(square, dir_mask)
		|| check_neighbors_white_n(square, dir_mask);
}

static inline
move_t* gen_pawn_black(move_t* moves, register piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_SW, Square_Rank1, Piece_Black, Piece_White);
	moves = gen_vector_pawn(moves, from, Vec_SE, Square_Rank1, Piece_Black, Piece_White);
	return gen_push_pawn(moves, from, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves) {
	if (piecemask & (1ull << Piece_EP)) {
		moves = gen_vector_ep(moves, Vec_NW, Piece_Black, Piece_White);
		moves = gen_vector_ep(moves, Vec_NE, Piece_Black, Piece_White);
	}
	return moves;
}

static inline
bool check_neighbors_black_s(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_diag(square, Vec_SW, Dir_SW, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_Black)
		|| check_vector_diag (square, Vec_SE, Dir_SE, dir_mask, Piece_Black));
}

static inline
bool check_neighbors_black_we(register square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_Black);
}

static inline
bool check_neighbors_black_n(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_pawn(square, Vec_NW, Dir_NW, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_N, Dir_N, dir_mask, Piece_Black)
		|| check_vector_pawn(square, Vec_NE, Dir_NE, dir_mask, Piece_Black));
}

static inline
bool check_neighbors_black(register square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_black_s(square, dir_mask)
		|| check_neighbors_black_we(square, dir_mask)
		|| check_neighbors_black_n(square, dir_mask);
}

static inline
move_t* gen_king(move_t* moves, register piece_square_t from, uint8_t color) {
	moves = gen_vector_king(moves, from, Vec_SW, color);
	moves = gen_vector_king(moves, from, Vec_S,  color);
	moves = gen_vector_king(moves, from, Vec_SE, color);
	moves = gen_vector_king(moves, from, Vec_W,  color);
	moves = gen_vector_king(moves, from, Vec_E,  color);
	moves = gen_vector_king(moves, from, Vec_NW, color);
	moves = gen_vector_king(moves, from, Vec_N,  color);
	moves = gen_vector_king(moves, from, Vec_NE, color);
	return moves;
}

static inline
bool check_king(register piece_square_t from, register square_t src) {
	register square_t dest = from.square;
	register uint8_t delta = abs(dest - src);
	return delta == Vec_E || delta == Vec_NW || delta == Vec_N || delta == Vec_NE;
}

static inline
move_t* gen_knight(move_t* moves, register piece_square_t from, uint8_t color) {
	moves = gen_vector_knight(moves, from, Vec_SSW, color);
	moves = gen_vector_knight(moves, from, Vec_SSE, color);
	moves = gen_vector_knight(moves, from, Vec_SWW, color);
	moves = gen_vector_knight(moves, from, Vec_SEE, color);
	moves = gen_vector_knight(moves, from, Vec_NWW, color);
	moves = gen_vector_knight(moves, from, Vec_NEE, color);
	moves = gen_vector_knight(moves, from, Vec_NNW, color);
	moves = gen_vector_knight(moves, from, Vec_NNE, color);
	return moves;
}

static inline
bool check_knight(register piece_square_t from, register square_t src) {
	register square_t dest = from.square;
	register uint8_t delta = abs(dest - src);
	return delta == Vec_NWW || delta == Vec_NEE || delta == Vec_NNW || delta == Vec_NNE;
}

static inline
move_t* gen_bishop(move_t* moves, register piece_square_t from, uint8_t color) {
	moves = gen_vector_slider(moves, from, Vec_SW, color);
	moves = gen_vector_slider(moves, from, Vec_SE, color);
	moves = gen_vector_slider(moves, from, Vec_NW, color);
	moves = gen_vector_slider(moves, from, Vec_NE, color);
	return moves;
}

static inline
bool check_bishop(register piece_square_t from, register square_t src, dir_mask_t dir_mask) {
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_rook(move_t* moves, register piece_square_t from, uint8_t color) {
	moves = gen_vector_slider(moves, from, Vec_S, color);
	moves = gen_vector_slider(moves, from, Vec_W, color);
	moves = gen_vector_slider(moves, from, Vec_E, color);
	moves = gen_vector_slider(moves, from, Vec_N, color);
	return moves;
}

static inline
bool check_rook(register piece_square_t from, register square_t src, dir_mask_t dir_mask) {
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_ortho(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_queen(move_t* moves, register piece_square_t from, uint8_t color) {
	moves = gen_vector_slider(moves, from, Vec_SW, color);
	moves = gen_vector_slider(moves, from, Vec_S,  color);
	moves = gen_vector_slider(moves, from, Vec_SE, color);
	moves = gen_vector_slider(moves, from, Vec_W,  color);
	moves = gen_vector_slider(moves, from, Vec_E,  color);
	moves = gen_vector_slider(moves, from, Vec_NW, color);
	moves = gen_vector_slider(moves, from, Vec_N,  color);
	moves = gen_vector_slider(moves, from, Vec_NE, color);
	return moves;
}

static inline
bool check_queen(register piece_square_t from, register square_t src, dir_mask_t dir_mask) {
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank, dir_mask)
		|| check_ortho(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_kings(move_t* moves, uint8_t color) {
	register piece_t piece = (Piece_King | color) & Piece_Index;
	return gen_king(moves, pieces[piece], color);
}

static inline
bool check_kings(register square_t square, uint8_t color) {
	register piece_t piece = (Piece_King | color) & Piece_Index;
	return check_king(pieces[piece], square);
}

static inline
move_t* gen_pawns_white(move_t* moves) {
	register piece_t piece = Piece_Pawn0;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Pawns; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_pawn_white(moves, pieces[piece]);
		}
	}
	return moves;
}

static inline
move_t* gen_pawns_black(move_t* moves) {
	register piece_t piece = Piece_Pawn0 | Piece_Black;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Pawns; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_pawn_black(moves, pieces[piece]);
		}
	}
	return moves;
}

static inline
move_t* gen_knights(move_t* moves, uint8_t color) {
	register piece_t piece = (Piece_Knight | color) & Piece_Index;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Knights; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_knight(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_knights(register square_t square, uint8_t color) {
	register piece_t piece = ((Piece_Knight + get_index(square)) | color) & Piece_Index;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Knights2; ++i, ++piece, mask <<= 1) {
		if ((piecemask & mask) && check_knight(pieces[piece], square)) {
			return true;
		}
	}
	return false;
}

static inline
move_t* gen_bishops(move_t* moves, uint8_t color) {
	register piece_t piece = (Piece_Bishop | color) & Piece_Index;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Bishops; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_bishop(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_bishops(register square_t square, dir_mask_t dir_mask, uint8_t color) {
	if (dir_mask & (DirMask_SW | DirMask_SE | DirMask_NW | DirMask_NE)) {
		register piece_t piece = ((Piece_Bishop + get_index(square)) | color) & Piece_Index;
		register uint64_t mask = 1ull << piece;
		for (uint8_t i = 0; i < Count_Bishops2; ++i, ++piece, mask <<= 1) {
			if ((piecemask & mask) && check_bishop(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
move_t* gen_rooks(move_t* moves, uint8_t color) {
	register piece_t piece = (Piece_Rook | color) & Piece_Index;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_rook(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_rooks(register square_t square, dir_mask_t dir_mask, uint8_t color) {
	if (dir_mask & (DirMask_S | DirMask_W | DirMask_E | DirMask_N)) {
		register piece_t piece = (Piece_Rook | color) & Piece_Index;
		register uint64_t mask = 1ull << piece;
		for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask <<= 1) {
			if ((piecemask & mask) && check_rook(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
move_t* gen_queens(move_t* moves, uint8_t color) {
	register piece_t piece = (Piece_Queen | color) & Piece_Index;
	register uint64_t mask = 1ull << piece;
	for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask <<= 1) {
		if (piecemask & mask) {
			moves = gen_queen(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_queens(register square_t square, dir_mask_t dir_mask, uint8_t color) {
	if (dir_mask) {
		register piece_t piece = (Piece_Queen | color) & Piece_Index;
		register uint64_t mask = 1ull << piece;
		for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask <<= 1) {
			if ((piecemask & mask) && check_queen(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
bool check_square_white(register square_t square) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_white(square, &dir_mask)
		|| check_knights(square, Piece_White)
		|| check_bishops(square, dir_mask, Piece_White)
		|| check_rooks(square, dir_mask, Piece_White)
		|| check_queens(square, dir_mask, Piece_White);
}

static inline
bool check_square_black(register square_t square) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_black(square, &dir_mask)
		|| check_knights(square, Piece_Black)
		|| check_bishops(square, dir_mask, Piece_Black)
		|| check_rooks(square, dir_mask, Piece_Black)
		|| check_queens(square, dir_mask, Piece_Black);
}

static inline
move_t* gen_white(move_t* moves) {
	moves = gen_kings(moves, Piece_White);
	moves = gen_pawns_white(moves);
	moves = gen_knights(moves, Piece_White);
	moves = gen_bishops(moves, Piece_White);
	moves = gen_rooks(moves, Piece_White);
	moves = gen_queens(moves, Piece_White);
	moves = gen_ep_white(moves);
	return moves;
}

static inline
move_t* gen_black(move_t* moves) {
	moves = gen_ep_black(moves);
	moves = gen_kings(moves, Piece_Black);
	moves = gen_pawns_black(moves);
	moves = gen_knights(moves, Piece_Black);
	moves = gen_bishops(moves, Piece_Black);
	moves = gen_rooks(moves, Piece_Black);
	moves = gen_queens(moves, Piece_Black);
	return moves;
}

static inline
bool check_white() {
	piece_t piece = Piece_King | Piece_Black;
	return check_square_white(pieces[piece].square);
}

static inline
bool check_black() {
	piece_t piece = Piece_King;
	return check_square_black(pieces[piece].square);
}

static inline
move_t* gen(move_t* moves) {
	return color == Piece_White
		? gen_white(moves)
		: gen_black(moves);
}

static inline
bool check() {
	return color == Piece_White
		? check_white()
		: check_black();
}

static inline
void clear_piece(register piece_square_t ps) {
	register piece_t piece = ps.piece & Piece_Index;
	piecemask &= ~(1ull << piece);
}

static inline
void set_piece(register piece_square_t ps) {
	register piece_t piece = ps.piece & Piece_Index;
	pieces[piece] = ps;
	piecemask |= (1ull << piece);
}

static inline
void clear_prim_from(register piece_square_t from) {
	squares[from.square] = 0x00;
	clear_piece(from);
}

static inline
void set_prim_from(register piece_square_t from) {
	squares[from.square] = from.piece;
	set_piece(from);
}

static inline
void clear_prim_to(register piece_square_t to) {
	squares[to.square] = 0x00;
	clear_piece(to);
}

static inline
void set_prim_to(register piece_square_t to) {
	to.piece |= Piece_Moved;
	squares[to.square] = to.piece;
	set_piece(to);
}

static inline
void clear_sec(register piece_square_t ps) {
	squares[ps.square] = 0x00;
	clear_piece(ps);
}

static inline
void set_sec(register piece_square_t ps) {
	squares[ps.square] = ps.piece;
	set_piece(ps);
}

static inline
void clear_ep() {
	piecemask &= ~(1ull << Piece_EP);
}

static inline
void move_make(register move_t move) {
	clear_ep();

	clear_sec(move.sec.from);
	set_sec(move.sec.to);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	color ^= Piece_Color;
}

static inline
void move_unmake(register move_t move) {
	color ^= Piece_Color;

	clear_prim_to(move.prim.to);
	set_prim_from(move.prim.from);
	clear_sec(move.sec.to);
	set_sec(move.sec.from);
}

uint64_t perft(move_t* moves, register uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	state_t state;
	if (!depth)
		return 1;
	pEnd = gen(moves);
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		state.ep = pieces[Piece_EP].square;
		move_make(*pCurr);
		if (!check())
			count += perft(pEnd, depth - 1);
		move_unmake(*pCurr);
		pieces[Piece_EP].square = state.ep;
	}
	return count;
}

void set_pieces() {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = squares[ps.square])) {
				set_piece(ps);
			}
		}
	}
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
				? piece_chars[(piece & (Piece_Type | Piece_Black)) >> Shift_Type]
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
	set_pieces();

	board_write(buffer);
	printf("%s\n", buffer);

	for (uint8_t depth = 0; depth <= max; ++depth) {
		uint64_t count = perft(moves, depth);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return 0;
}
