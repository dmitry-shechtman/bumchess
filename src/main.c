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

#ifdef _MSC_VER
#include <intrin.h>
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
	Shift_Castling    =  1,
	Shift_Odd         =  1,
	Shift_Type        =  2,
	Shift_Row         =  3,
	Shift_File        =  3,
	Shift_Rank        =  4,

	Shift_EP_Index    =  4,
	Shift_Square      =  8,
	Shift_Piece_EP    = 15,
};

enum Piece {
	Piece_Odd      = 0x02,
	Piece_Index2   = 0x03,
	Piece_Index3   = 0x07,

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

	Piece_Guard    = Piece_Black - 1,
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
	Square_FileA       = 0x00,
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
	Count_Colors    =   2,
	Count_Castlings =   4,
	Count_Type4     =  16,

	Count_Ranks     =   8,
	Count_Files     =   8,
	Count_Squares   = 128,

	Count_Pawns    =    8,
	Count_Knights  =    4,
	Count_Bishops  =    4,
	Count_Rooks    =    4,
	Count_Queens   =    3,
	Count_Pieces   =   64,
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

enum Char {
	Char_Zero = '0',
	Char_Nine = '9',

	Char_Rank = '1',
	Char_File = 'a',
};

struct {
	union {
		piece_t squares[Count_Squares];
		uint64_t rows[16];
	};
	piece_square_t pieces[Count_Pieces];
	piece_t color;
} board;

const move_t nullmove = {
	.prim = {
		.from = { 0x0800 },
		.to = { 0x0800 }
	},
	.sec = {
		.from = { 0x0800 },
		.to = { 0x0800 }
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

char castling_chars[Count_Castlings] = "KQkq";

square_t castling_squares[Count_Castlings] = {
	Square_FileH | Square_Rank1, Square_FileA | Square_Rank1,
	Square_FileH | Square_Rank8, Square_FileA | Square_Rank8
};

typedef struct {
	const char* fen;
	uint8_t  min;
	uint8_t  max;
	uint8_t  div;
	uint64_t result;
} params_t;

static inline
piece_t find_next(uint64_t* mask) {
#ifdef _MSC_VER
	uint32_t index;
	_BitScanForward64(&index, *mask);
#else
	uint8_t index = __builtin_ctzl(*mask);
#endif
	*mask &= (*mask - 1);
	return index;
}

static inline
piece_t get_square(register square_t square) {
	return board.squares[square];
}

static inline
void clear_square(register piece_square_t ps) {
	board.squares[ps.square] = 0x00;
}

static inline
void set_square(register piece_square_t ps) {
	board.squares[ps.square] = ps.piece;
}

static inline
piece_square_t get_piece(register piece_t piece) {
	return board.pieces[piece];
}

static inline
uint64_t clear_piece(register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	return piecemask &= ~(1ull << piece);
}

static inline
uint64_t set_piece(register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	board.pieces[piece] = ps;
	return piecemask |= (1ull << piece);
}

static inline
uint8_t get_index2(register square_t square) {
	return ((square ^ (square >> Shift_Rank)) & 1) << Shift_Odd;
}

static inline
piece_square_t find_index_to(register piece_square_t ps, register const uint64_t piecemask) {
	for (uint64_t mask = piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

static inline
piece_square_t find_index_to_bishop(register piece_square_t ps, register uint64_t piecemask) {
	ps.value += get_index2(ps.square);
	return find_index_to(ps, piecemask);
}

piece_square_t find_index_error(piece_square_t ps) {
	ps.value = 0;
	return ps;
}

piece_square_t find_index_king(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return (ps2.value & Piece_Index2)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_pawn(piece_square_t ps, const uint64_t piecemask) {
	ps.piece |= (ps.square & Square_File);
	return (piecemask & (1ull << (ps.value & Piece_Index)))
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_bishop(piece_square_t ps, const uint64_t piecemask) {
	ps.value += get_index2(ps.square);
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & (Piece_Type | Piece_Odd))
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_rook(piece_square_t ps, const uint64_t piecemask) {
	ps.piece |= (piecemask >> (Piece_King | (ps.piece & Piece_Black))) & 1;
	return (piecemask & (1ull << (ps.value & Piece_Index)))
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_moved_pawn(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_TypePawn)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_other(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_Type)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index(piece_square_t ps, const uint64_t piecemask) {
	switch (ps.piece & Piece_Type) {
	case Piece_King:
		return find_index_king(ps, piecemask);
	case Piece_Pawn0:
		return find_index_pawn(ps, piecemask);
	case Piece_Rook:
		return find_index_rook(ps, piecemask);
	default:
		return find_index_error(ps);
	}
}

piece_square_t find_index_moved(piece_square_t ps, const uint64_t piecemask) {
	switch (ps.piece & Piece_Type) {
	case Piece_King:
		return find_index_king(ps, piecemask);
	case Piece_Pawn0:
		return find_index_moved_pawn(ps, piecemask);
	case Piece_Bishop:
		return find_index_bishop(ps, piecemask);
	default:
		return find_index_other(ps, piecemask);
	}
}

static inline
move_t* gen_promo(move_t* moves, register move_t move, register piece_square_t to,
	register const uint64_t piecemask, const piece_t piece, const uint8_t color)
{
	to.piece = piece | color | Piece_Moved;
	move.prim.to = find_index_to(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_bishop(move_t* moves, register move_t move, register piece_square_t to,
	register const uint64_t piecemask, const uint8_t color)
{
	to.piece = Piece_Bishop | color | Piece_Moved;
	move.prim.to = find_index_to_bishop(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to,
	register const uint64_t piecemask, const uint8_t promo, const uint8_t color)
{
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo(moves, move, to, piecemask, Piece_Knight, color);
		moves = gen_promo_bishop(moves, move, to, piecemask, color);
		moves = gen_promo(moves, move, to, piecemask, Piece_Rook, color);
		moves = gen_promo(moves, move, to, piecemask, Piece_Queen, color);
		return moves;
	}
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_push2_pawn(move_t* moves, register piece_square_t from, register piece_square_t to, register piece_square_t from2,
	const uint8_t color2)
{
	register uint64_t row = board.rows[to.square >> Shift_Row];
	register piece_t left = !((to.square - 1) & Square_FileInvalid)
		? (piece_t)(row >> (((to.square - 1) & Square_File) << Shift_File))
		: 0;
	register piece_t piece = (piece_t)(row >> ((to.square & Square_File) << Shift_File));
	register piece_t right = !((to.square + 1) & Square_FileInvalid)
		? (piece_t)(row >> (((to.square + 1) & Square_File) << Shift_File))
		: 0;
	if (!piece) {
		register move_t move = {
			.prim = {
				.from = { from.value |
					(((left & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color2)) << Shift_Piece_EP) },
				.to = { to.value |
					(((right & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color2)) << Shift_Piece_EP) }
			},
			.sec = {
				.from = { (((left & Piece_Index3) | ((right & Piece_Index3) << Shift_EP_Index)) << Shift_Square) | 0x0800 },
				.to = { from2.value | Piece_EP | 0x0800 }
			}
		};
		*moves++ = move;
	}
	return moves;
}

static inline
move_t* gen_push_pawn(move_t* moves, register piece_square_t from, register const uint64_t piecemask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!(from2.piece = get_square(from2.square = to.square += vector))) {
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
		moves = gen_promo_pawn(moves, move, to, piecemask, promo, color);
		if (!(from.piece & Piece_Moved)) {
			to.square += vector;
			moves = gen_push2_pawn(moves, from, to, from2, color2);
		}
	}
	return moves;
}

static inline
move_t* gen_vector_pawn(move_t* moves, register piece_square_t from, register const uint64_t piecemask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& ((from2.piece = get_square(from2.square = to.square)) & color2)) {
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
			moves = gen_promo_pawn(moves, move, to, piecemask, promo, color);
	}
	return moves;
}

static inline
move_t* gen_vector_ep(move_t* moves, register piece_square_t ps, register uint8_t piece, register square_t square,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	if (ps.value & 0x8000) {
		register piece_square_t to = {
			.piece = (piece & Piece_Index3) | Piece_Pawn0 | color | Piece_Moved,
			.square = square & ~Square_FileInvalid
		};
		register move_t move = {
			.prim = {
				.from = {
					.piece = to.piece,
					.square = to.square + vector
				},
				.to = to
			},
			.sec = {
				.from = {
					.piece = (to.square & Piece_Index3) | Piece_Pawn0 | color2 | Piece_Moved,
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
move_t* gen_vector_leaper(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(from2.square = to.square)) & color)) {
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
bool check_square(register square_t square,
	const type_mask_t type_mask, const uint8_t dir, dir_mask_t* dir_mask, const uint8_t color)
{
	register piece_t piece = get_square(square);
	if (piece & color) {
		return type_mask & (1 << (piece & Piece_Type));
	}
	*dir_mask |= (!piece ? (1 << dir) : 0);
	return 0;
}

static inline
bool check_square_knight(register square_t square,
	const uint8_t color)
{
	register piece_t piece = (square & Square_Invalid) ? 0 : get_square(square);
	return (piece & (Piece_Type | Piece_Color)) == (Piece_Knight | color);
}

static inline
bool check_vector(register square_t square,
	const type_mask_t type_mask, const vector_t vector, const uint8_t dir, dir_mask_t* dir_mask, const uint8_t color)
{
	return !((square += vector) & Square_Invalid)
		&& check_square(square, type_mask, dir, dir_mask, color);
}

static inline
bool check_vector_knight(register square_t square,
	const vector_t vector, const uint8_t color)
{
	return check_square_knight(square += vector, color);
}

static inline
bool check_vector_pawn(register square_t square,
	const vector_t vector, const uint8_t dir, dir_mask_t* dir_mask, const uint8_t color)
{
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King | TypeMask_Pawn,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_diag(register square_t square,
	const vector_t vector, const uint8_t dir, dir_mask_t* dir_mask, const uint8_t color)
{
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_ortho(register square_t square,
	const vector_t vector, const uint8_t dir, dir_mask_t* dir_mask, const uint8_t color)
{
	return check_vector(square, TypeMask_Queen | TypeMask_Rook | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
move_t* gen_vector_slider(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2 = {0};
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(from2.square = to.square)) & color)) {
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
bool check_vector_slider(register square_t src, register square_t dest,
	const vector_t vector, const uint8_t dir, register const dir_mask_t dir_mask)
{
	if (!(dir_mask & (1 << dir))) {
		return false;
	}
	for (src += vector; !get_square(src += vector); );
	return src == dest;
}

static inline
bool check_vert(register square_t src, register square_t dest, register int8_t drank,
	register const dir_mask_t dir_mask)
{
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_N, Dir_N, dir_mask)
		: check_vector_slider(src, dest, Vec_S, Dir_S, dir_mask);
}

static inline
bool check_horiz(register square_t src, register square_t dest, register int8_t dfile,
	register const dir_mask_t dir_mask)
{
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_E, Dir_E, dir_mask)
		: check_vector_slider(src, dest, Vec_W, Dir_W, dir_mask);
}

static inline
bool check_diag1(register square_t src, register square_t dest, register int8_t drank,
	register const dir_mask_t dir_mask)
{
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_NE, Dir_NE, dir_mask)
		: check_vector_slider(src, dest, Vec_SW, Dir_SW, dir_mask);
}

static inline
bool check_diag2(register square_t src, register square_t dest, register int8_t dfile,
	register const dir_mask_t dir_mask)
{
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_SE, Dir_SE, dir_mask)
		: check_vector_slider(src, dest, Vec_NW, Dir_NW, dir_mask);
}

static inline
bool check_ortho(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank,
	register const dir_mask_t dir_mask)
{
	return !dfile
		? check_vert(src, dest, drank, dir_mask)
		: !drank
			? check_horiz(src, dest, dfile, dir_mask)
			: false;
}

static inline
bool check_diag(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank,
	register const dir_mask_t dir_mask)
{
	return dfile == drank
		? check_diag1(src, dest, drank, dir_mask)
		: dfile == -drank
			? check_diag2(src, dest, dfile, dir_mask)
			: false;
}

static inline
move_t* gen_pawn_white(move_t* moves, register piece_square_t from, register const uint64_t piecemask) {
	moves = gen_vector_pawn(moves, from, piecemask, Vec_NW, Square_Rank8, Piece_White, Piece_Black);
	moves = gen_vector_pawn(moves, from, piecemask, Vec_NE, Square_Rank8, Piece_White, Piece_Black);
	return gen_push_pawn(moves, from, piecemask, Vec_N, Square_Rank8, Piece_White, Piece_Black);
}

static inline
move_t* gen_ep_white(move_t* moves, register const move_t move) {
	moves = gen_vector_ep(moves, move.prim.from,
		move.sec.from.square, move.sec.to.square, Vec_SW, Piece_White, Piece_Black);
	moves = gen_vector_ep(moves, move.prim.to,
		move.sec.from.square >> Shift_EP_Index, move.sec.to.square, Vec_SE, Piece_White, Piece_Black);
	return moves;
}

static inline
bool check_neighbors_white_ss(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) > Square_Rank2
		&& (check_vector_knight(square, Vec_SSW, Piece_White)
			|| check_vector_knight(square, Vec_SSE, Piece_White));
}

static inline
bool check_neighbors_white_s(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_knight(square, Vec_SWW, Piece_White)
			|| check_vector_pawn (square, Vec_SW, Dir_SW, dir_mask, Piece_White)
			|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_White)
			|| check_vector_pawn (square, Vec_SE, Dir_SE, dir_mask, Piece_White)
			|| check_vector_knight(square, Vec_SEE, Piece_White));
}

static inline
bool check_neighbors_white_we(register square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_White);
}

static inline
bool check_neighbors_white_n(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_knight(square, Vec_NWW, Piece_White)
			|| check_vector_diag (square, Vec_NW, Dir_NW, dir_mask, Piece_White)
			|| check_vector_ortho(square, Vec_N,  Dir_N,  dir_mask, Piece_White)
			|| check_vector_diag (square, Vec_NE, Dir_NE, dir_mask, Piece_White)
			|| check_vector_knight(square, Vec_NEE, Piece_White));
}

static inline
bool check_neighbors_white_nn(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) < Square_Rank7
		&& (check_vector_knight(square, Vec_NNW, Piece_White)
			|| check_vector_knight(square, Vec_NNE, Piece_White));
}

static inline
bool check_neighbors_white(register square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_white_ss(square, dir_mask)
		|| check_neighbors_white_s(square, dir_mask)
		|| check_neighbors_white_we(square, dir_mask)
		|| check_neighbors_white_n(square, dir_mask)
		|| check_neighbors_white_nn(square, dir_mask);
}

static inline
move_t* gen_pawn_black(move_t* moves, register piece_square_t from, register const uint64_t piecemask) {
	moves = gen_vector_pawn(moves, from, piecemask, Vec_SW, Square_Rank1, Piece_Black, Piece_White);
	moves = gen_vector_pawn(moves, from, piecemask, Vec_SE, Square_Rank1, Piece_Black, Piece_White);
	return gen_push_pawn(moves, from, piecemask, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves, register const move_t move) {
	moves = gen_vector_ep(moves, move.prim.from,
		move.sec.from.square, move.sec.to.square, Vec_NW, Piece_Black, Piece_White);
	moves = gen_vector_ep(moves, move.prim.to,
		move.sec.from.square >> Shift_EP_Index, move.sec.to.square, Vec_NE, Piece_Black, Piece_White);
	return moves;
}

static inline
bool check_neighbors_black_ss(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) > Square_Rank2
		&& (check_vector_knight(square, Vec_SSW, Piece_Black)
			|| check_vector_knight(square, Vec_SSE, Piece_Black));
}

static inline
bool check_neighbors_black_s(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_knight(square, Vec_SWW, Piece_Black)
			|| check_vector_diag (square, Vec_SW, Dir_SW, dir_mask, Piece_Black)
			|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_Black)
			|| check_vector_diag (square, Vec_SE, Dir_SE, dir_mask, Piece_Black)
			|| check_vector_knight(square, Vec_SEE, Piece_Black));
}

static inline
bool check_neighbors_black_we(register square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_Black);
}

static inline
bool check_neighbors_black_n(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_knight(square, Vec_NWW, Piece_Black)
			|| check_vector_pawn (square, Vec_NW, Dir_NW, dir_mask, Piece_Black)
			|| check_vector_ortho(square, Vec_N, Dir_N, dir_mask, Piece_Black)
			|| check_vector_pawn (square, Vec_NE, Dir_NE, dir_mask, Piece_Black)
			|| check_vector_knight(square, Vec_NEE, Piece_Black));
}

static inline
bool check_neighbors_black_nn(register square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) < Square_Rank7
		&& (check_vector_knight(square, Vec_NNW, Piece_Black)
			|| check_vector_knight(square, Vec_NNE, Piece_Black));
}

static inline
bool check_neighbors_black(register square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_black_ss(square, dir_mask)
		|| check_neighbors_black_s(square, dir_mask)
		|| check_neighbors_black_we(square, dir_mask)
		|| check_neighbors_black_n(square, dir_mask)
		|| check_neighbors_black_nn(square, dir_mask);
}

static inline
move_t* gen_king(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
	moves = gen_vector_leaper(moves, from, Vec_SW, color);
	moves = gen_vector_leaper(moves, from, Vec_S,  color);
	moves = gen_vector_leaper(moves, from, Vec_SE, color);
	moves = gen_vector_leaper(moves, from, Vec_W,  color);
	moves = gen_vector_leaper(moves, from, Vec_E,  color);
	moves = gen_vector_leaper(moves, from, Vec_NW, color);
	moves = gen_vector_leaper(moves, from, Vec_N,  color);
	moves = gen_vector_leaper(moves, from, Vec_NE, color);
	return moves;
}

static inline
move_t* gen_knight(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
	moves = gen_vector_leaper(moves, from, Vec_SSW, color);
	moves = gen_vector_leaper(moves, from, Vec_SSE, color);
	moves = gen_vector_leaper(moves, from, Vec_SWW, color);
	moves = gen_vector_leaper(moves, from, Vec_SEE, color);
	moves = gen_vector_leaper(moves, from, Vec_NWW, color);
	moves = gen_vector_leaper(moves, from, Vec_NEE, color);
	moves = gen_vector_leaper(moves, from, Vec_NNW, color);
	moves = gen_vector_leaper(moves, from, Vec_NNE, color);
	return moves;
}

static inline
move_t* gen_bishop(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
	moves = gen_vector_slider(moves, from, Vec_SW, color);
	moves = gen_vector_slider(moves, from, Vec_SE, color);
	moves = gen_vector_slider(moves, from, Vec_NW, color);
	moves = gen_vector_slider(moves, from, Vec_NE, color);
	return moves;
}

static inline
bool check_bishop(register piece_square_t from, register square_t src,
	register const dir_mask_t dir_mask)
{
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_rook(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
	moves = gen_vector_slider(moves, from, Vec_S, color);
	moves = gen_vector_slider(moves, from, Vec_W, color);
	moves = gen_vector_slider(moves, from, Vec_E, color);
	moves = gen_vector_slider(moves, from, Vec_N, color);
	return moves;
}

static inline
bool check_rook(register piece_square_t from, register square_t src,
	register const dir_mask_t dir_mask)
{
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_ortho(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_queen(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
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
bool check_queen(register piece_square_t from, register square_t src,
	register const dir_mask_t dir_mask)
{
	register square_t dest = from.square;
	register int8_t dfile = (dest & Square_File) - (src & Square_File);
	register int8_t drank = (dest >> Shift_Rank) - (src >> Shift_Rank);
	return check_diag(src, dest, dfile, drank, dir_mask)
		|| check_ortho(src, dest, dfile, drank, dir_mask);
}

static inline
move_t* gen_kings(move_t* moves,
	const uint8_t color)
{
	register piece_t piece = Piece_King + (color & Piece_Black);
	return gen_king(moves, get_piece(piece), color);
}

static inline
move_t* gen_pawns_white(move_t* moves, register const uint64_t piecemask, uint64_t* mask, piece_t* piece) {
	for (; *piece < Piece_Pawn0 + Count_Pawns; *piece = find_next(mask)) {
		moves = gen_pawn_white(moves, get_piece(*piece), piecemask);
	}
	return moves;
}

static inline
move_t* gen_pawns_black(move_t* moves, register const uint64_t piecemask, uint64_t* mask, piece_t* piece) {
	for (; *piece < Piece_Pawn0 + Count_Pawns + Piece_Black; *piece = find_next(mask)) {
		moves = gen_pawn_black(moves, get_piece(*piece), piecemask);
	}
	return moves;
}

static inline
move_t* gen_knights(move_t* moves, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	for (; *piece < (Piece_Knight + Count_Knights + (color & Piece_Black)); *piece = find_next(mask)) {
		moves = gen_knight(moves, get_piece(*piece), color);
	}
	return moves;
}

static inline
move_t* gen_bishops(move_t* moves, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	for (; *piece < (Piece_Bishop + Count_Bishops + (color & Piece_Black)); *piece = find_next(mask)) {
		moves = gen_bishop(moves, get_piece(*piece), color);
	}
	return moves;
}

static inline
bool check_bishops(register square_t square, uint64_t* mask, piece_t* piece,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	for (; *piece < (Piece_Bishop + Count_Bishops + (color & Piece_Black)); *piece = find_next(mask)) {
		if (check_bishop(get_piece(*piece), square, dir_mask)) {
			return true;
		}
	}
	return false;
}

static inline
move_t* gen_rooks(move_t* moves, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	for (; *piece < (Piece_Rook + Count_Rooks + (color & Piece_Black)); *piece = find_next(mask)) {
		moves = gen_rook(moves, get_piece(*piece), color);
	}
	return moves;
}

static inline
bool check_rooks(register square_t square, uint64_t* mask, piece_t* piece,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	for (; *piece < (Piece_Rook + Count_Rooks + (color & Piece_Black)); *piece = find_next(mask)) {
		if (check_rook(get_piece(*piece), square, dir_mask)) {
			return true;
		}
	}
	return false;
}

static inline
move_t* gen_queens(move_t* moves, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	for (; *piece < (Piece_Queen + Count_Queens + (color & Piece_Black)); *piece = find_next(mask)) {
		moves = gen_queen(moves, get_piece(*piece), color);
	}
	return moves;
}

static inline
bool check_queens(register square_t square, uint64_t* mask, piece_t* piece,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	for (; *piece < (Piece_Queen + Count_Queens + (color & Piece_Black)); *piece = find_next(mask)) {
		if (check_queen(get_piece(*piece), square, dir_mask)) {
			return true;
		}
	}
	return false;
}

static inline
move_t* gen_pieces(move_t* moves, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	moves = gen_knights(moves, mask, piece, color);
	moves = gen_bishops(moves, mask, piece, color);
	moves = gen_rooks(moves, mask, piece, color);
	moves = gen_queens(moves, mask, piece, color);
	return moves;
}

static inline
bool check_sliders(register square_t square, register const uint64_t piecemask,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	piece_t piece = Piece_Bishop + (color & Piece_Black) + (get_index2(square) ^ Piece_Odd);
	uint64_t mask = piecemask & (0xFFF00000ull << (color & Piece_Black))
		& ~((1ull << piece) | (1ull << (piece + 1)));
	piece = find_next(&mask);
	return dir_mask && (check_bishops(square, &mask, &piece, dir_mask, color)
		|| check_rooks(square, &mask, &piece, dir_mask, color)
		|| check_queens(square, &mask, &piece, dir_mask, color));
}

static inline
bool check_to_white(register square_t square, register const uint64_t piecemask) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_white(square, &dir_mask)
		|| check_sliders(square, piecemask, dir_mask, Piece_White);
}

static inline
bool check_to_black(register square_t square, register const uint64_t piecemask) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_black(square, &dir_mask)
		|| check_sliders(square, piecemask, dir_mask, Piece_Black);
}

static inline
move_t* gen_white(move_t* moves, register const uint64_t piecemask, register const move_t move) {
	uint64_t mask = piecemask & 0x00000000FFFFFF00;
	piece_t piece = find_next(&mask);
	moves = gen_kings(moves, Piece_White);
	moves = gen_pawns_white(moves, piecemask, &mask, &piece);
	moves = gen_pieces(moves, &mask, &piece, Piece_White);
	moves = gen_ep_white(moves, move);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
move_t* gen_black(move_t* moves, register const uint64_t piecemask, register const move_t move) {
	uint64_t mask = piecemask & 0xFFFFFF0000000000;
	piece_t piece = find_next(&mask);
	moves = gen_kings(moves, Piece_Black);
	moves = gen_pawns_black(moves, piecemask, &mask, &piece);
	moves = gen_pieces(moves, &mask, &piece, Piece_Black);
	moves = gen_ep_black(moves, move);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
bool check_white(register const uint64_t piecemask) {
	piece_t piece = Piece_King | Piece_Black;
	return check_to_white(get_piece(piece).square, piecemask);
}

static inline
bool check_black(register const uint64_t piecemask) {
	piece_t piece = Piece_King;
	return check_to_black(get_piece(piece).square, piecemask);
}

static inline
move_t* gen(move_t* moves, register const uint64_t piecemask, register const move_t move) {
	return board.color == Piece_White
		? gen_white(moves, piecemask, move)
		: gen_black(moves, piecemask, move);
}

static inline
bool check(register const uint64_t piecemask) {
	return board.color == Piece_White
		? check_white(piecemask)
		: check_black(piecemask);
}

static inline
uint64_t set_init(register piece_square_t ps, register uint64_t piecemask) {
	set_square(ps);
	return set_piece(ps, piecemask);
}

static inline
uint64_t clear_prim_from(register piece_square_t from, register uint64_t piecemask) {
	from.square &= ~Square_RankInvalid;
	clear_square(from);
	return clear_piece(from, piecemask);
}

static inline
uint64_t set_prim_from(register piece_square_t from, register uint64_t piecemask) {
	from.square &= ~Square_RankInvalid;
	set_square(from);
	return set_piece(from, piecemask);
}

static inline
uint64_t clear_prim_to(register piece_square_t to, register uint64_t piecemask) {
	to.square &= ~Square_RankInvalid;
	clear_square(to);
	return clear_piece(to, piecemask);
}

static inline
uint64_t set_prim_to(register piece_square_t to, register uint64_t piecemask) {
	to.square &= ~Square_RankInvalid;
	to.piece |= Piece_Moved;
	set_square(to);
	return set_piece(to, piecemask);
}

static inline
uint64_t clear_sec(register piece_square_t ps, register uint64_t piecemask) {
	clear_square(ps);
	return clear_piece(ps, piecemask);
}

static inline
uint64_t set_sec(register piece_square_t ps, register uint64_t piecemask) {
	set_square(ps);
	return set_piece(ps, piecemask);
}

static inline
uint64_t move_make(register move_t move, register uint64_t piecemask) {
	piecemask = clear_sec(move.sec.from, piecemask);
	piecemask = set_sec(move.sec.to, piecemask);
	piecemask = clear_prim_from(move.prim.from, piecemask);
	piecemask = set_prim_to(move.prim.to, piecemask);

	board.color ^= Piece_Color;

	return piecemask;
}

static inline
uint64_t move_unmake(register move_t move, register uint64_t piecemask) {
	board.color ^= Piece_Color;

	piecemask = clear_prim_to(move.prim.to, piecemask);
	piecemask = set_prim_from(move.prim.from, piecemask);
	piecemask = clear_sec(move.sec.to, piecemask);
	piecemask = set_sec(move.sec.from, piecemask);

	return piecemask;
}

uint64_t perft_opt(move_t* moves, register uint64_t piecemask, register const move_t move, register uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	pEnd = gen(moves, piecemask, move);
	--depth;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		piecemask = move_make(*pCurr, piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(piecemask)) {
			count += depth
				? perft_opt(pEnd, piecemask, *pCurr, depth)
				: 1;
		}
		piecemask = move_unmake(*pCurr, piecemask);
	}
	return count;
}

extern char buffer[1024];

uint64_t perft(move_t* moves, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* str);
char* move_write(char* str, move_t move);

uint64_t perft_divide(move_t* moves, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* str) {
	str = move_write(str, move);
	*str++ = ' ';
	uint64_t count = perft(moves, piecemask, move, depth, div, str);
	*str = 0;
	printf("%s%11" PRIu64 "\n", buffer, count);
	return count;
}

uint64_t perft(move_t* moves, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* str) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	if (!depth)
		return 1;
	if (!div)
		return perft_opt(moves, piecemask, move, depth);
	pEnd = gen(moves, piecemask, move);
	--depth;
	--div;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		piecemask = move_make(*pCurr, piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(piecemask))
			count += perft_divide(pEnd, piecemask, *pCurr, depth, div, str);
		piecemask = move_unmake(*pCurr, piecemask);
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
	return find_char(c, castling_chars, Count_Castlings);
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
	for (uint8_t i = 0; i < c - Char_Zero; ++i) {
		clear_square(*ps);
	}
	ps->square += c - Char_Zero;
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
	board.color = color_values[i];
	return str;
}

char* fen_write_color(char* str) {
	*str++ = color_chars[board.color == Piece_Black];
	return str;
}

const char* fen_read_castling(const char* str) {
	char c = *str++;
	uint8_t i;
	piece_square_t ps;
	if ((i = find_castling(c)) == Count_Castlings
		|| (ps.piece = get_square(ps.square = castling_squares[i])) != (Piece_Rook | Piece_Moved | color_values[i >> Shift_Castling])) {
			return fen_read_error(c);
	}
	ps.piece &= ~Piece_Moved;
	set_square(ps);
	return str;
}

char* fen_write_castling(char* str, uint8_t i) {
	piece_t piece = get_square(castling_squares[i]);
	if (piece && !(piece & Piece_Moved)) {
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

const char* fen_read_ep_square(const char* str, uint64_t* piecemask, move_t* move) {
	*piecemask |= (1ull << Piece_EP);
	str = fen_read_square(str, &move->sec.to.square);
	move->prim.to.square = move->sec.to.square ^ Square_Rank2;
	move->prim.to.piece = get_square(move->prim.to.square);
	clear_square(move->prim.to);
	gen_push2_pawn(move, move->prim.from, move->prim.to, move->sec.to, board.color);
	set_square(move->prim.to);
	return str;
}

char* fen_write_ep_square(char* str, move_t move) {
	return square_write(str, move.sec.to.square);
}

const char* fen_read_castlings(const char* str) {
	return *str != '-'
		? fen_read_castling_chars(str)
		: ++str;
}

char* fen_write_castlings(char* str) {
	char* start = str;
	for (uint8_t i = 0; i < Count_Castlings; ++i) {
		str = fen_write_castling(str, i);
	}
	if (str == start) {
		*str++ = '-';
	}
	return str;
}

const char* fen_read_ep(const char* str, uint64_t* piecemask, move_t* move) {
	return *str != '-'
		? fen_read_ep_square(str, piecemask, move)
		: ++str;
}

char* fen_write_ep(char* str, uint64_t piecemask, move_t move) {
	if (!(piecemask & (1ull << Piece_EP))) {
		*str++ = '-';
	} else {
		str = fen_write_ep_square(str, move);
	}
	return str;
}

const char* fen_read(const char* str, uint64_t* piecemask, move_t* move) {
	*move = nullmove;
	*piecemask = (1ull << Piece_Guard) | (1ull << (Piece_Guard + Piece_Black));
	if (!(str = fen_read_squares(str))
		|| !(str = fen_read_char(str, ' ')) || !(str = fen_read_color(str))
		|| ((*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_castlings(str))
		|| (*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_ep(str, piecemask, move)))))))) {
		return 0;
	}
	return str;
}

char* fen_write(char* str, uint64_t piecemask, move_t move) {
	str = fen_write_squares(str);
	*str++ = ' ';
	str = fen_write_color(str);
	*str++ = ' ';
	str = fen_write_castlings(str);
	*str++ = ' ';
	str = fen_write_ep(str, piecemask, move);
	return str;
}

uint64_t set_pieces_unmoved(square_t ep_pawn, uint64_t piecemask) {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(ps.square))
				&& (!(ps.piece & Piece_Moved) || ps.square == ep_pawn)) {
					if (!(ps2 = find_index(ps, piecemask)).value) {
						fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
						return 0;
					}
					piecemask = set_init(ps2, piecemask);
			}
		}
	}
	return piecemask;
}

uint64_t set_pieces_moved(square_t ep_pawn, uint64_t piecemask) {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if (((ps.piece = get_square(ps.square)) & Piece_Moved)
				&& ps.square != ep_pawn) {
					if (!(ps2 = find_index_moved(ps, piecemask)).value) {
						fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
						return 0;
					}
					piecemask = set_init(ps2, piecemask);
			}
		}
	}
	return piecemask;
}

bool validate_kings(uint64_t piecemask) {
	for (uint8_t i = 0; i < Count_Colors; ++i) {
		piece_t piece = Piece_King | color_values[i];
		if (!(piecemask & (1ull << (piece & Piece_Index)))) {
			fprintf(stderr, "Missing %c.\n", get_piece_char(piece));
			return false;
		}
	}
	return true;
}

bool validate_ep(square_t ep_pawn, uint64_t piecemask, move_t move) {
	if ((piecemask & (1ull << Piece_EP))
		&& ((move.sec.to.square & Square_Rank) != color_ranks[board.color == Piece_Black]
			|| get_square(move.sec.to.square)
			|| (get_square(ep_pawn) & (Piece_TypePawn | Piece_Color)) != (Piece_Pawn0 | (board.color ^ Piece_Color)))) {
				fprintf(stderr, "Invalid e.p. square.\n");
				return false;
	}
	return true;
}

bool validate_check(uint64_t piecemask) {
	if (check(piecemask)) {
		fprintf(stderr, "Illegal position.\n");
		return false;
	}
	return true;
}

bool validate(square_t ep_pawn, uint64_t piecemask, move_t move) {
	return validate_kings(piecemask)
		&& validate_ep(ep_pawn, piecemask, move)
		&& validate_check(piecemask);
}

uint64_t set_pieces(uint64_t piecemask, move_t move) {
	move.sec.to.square &= ~Square_FileInvalid;
	square_t ep_pawn = piecemask & (1ull << Piece_EP)
		? move.sec.to.square ^ Square_Rank2
		: Square_FileInvalid;
	return (piecemask = set_pieces_unmoved(ep_pawn, piecemask))
		&& (piecemask = set_pieces_moved(ep_pawn, piecemask))
		&& validate(ep_pawn, piecemask, move)
			? piecemask
			: 0;
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

char buffer[1024];
move_t moves[1024];

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
	params_t params;
	uint64_t count = 0;

	if (!args_read(argc, argv, &params)) {
		printf("Usage: perft [<fen>] [<depth> [<result>]] [-d<divide>] [-l]\n");
		return -1;
	}

	move_t move;
	uint64_t piecemask;
	if (!fen_read(params.fen, &piecemask, &move)
		|| !(piecemask = set_pieces(piecemask, move))) {
			return 1;
	}

	if (params.result == UINT64_MAX) {
		board_write(buffer);
		printf("%s\n", buffer);
	} else {
		fen_write(buffer, piecemask, move);
		printf("\nPosition: %s\n", buffer);
	}

	for (uint8_t depth = params.min; depth <= params.max; ++depth) {
		count = perft(moves, piecemask, move, depth, params.div, buffer);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return params.result == UINT64_MAX || count == params.result
		? 0
		: 2;
}
