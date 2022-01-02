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
#include <string.h>

#ifdef _MSC_VER
#include <intrin.h>
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
	Count_Ranks    =   8,
	Count_Files    =   8,
	Count_Squares  = 128,

	Count_Pawns    =   8,
	Count_Knights  =   4,
	Count_Bishops  =   4,
	Count_Rooks    =   4,
	Count_Queens   =   3,
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
	square_t square;
} ep_state_t;

typedef struct {
	ep_state_t ep;
} state_t;

piece_t squares[Count_Squares];
piece_square_t pieces[Count_Pieces];
state_t state;
piece_t color;

char piece_chars[] = ":KPPNBRQ;kppnbrq";

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

uint64_t board_init() {
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
	return (1ull << Piece_Guard) | (1ull << (Piece_Guard + Piece_Black));
}

static inline
piece_t get_square(register square_t square) {
	return squares[square];
}

static inline
void clear_square(register piece_square_t ps) {
	squares[ps.square] = 0x00;
}

static inline
void set_square(register piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

static inline
piece_square_t get_piece(register piece_t piece) {
	return pieces[piece];
}

static inline
uint64_t clear_piece(register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	return piecemask &= ~(1ull << piece);
}

static inline
uint64_t set_piece(register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	pieces[piece] = ps;
	return piecemask |= (1ull << piece);
}

static inline
uint8_t get_index2_knight(register square_t square) {
	return (((square ^ (square >> Shift_Rank)) & 1) ^ 1) << Shift_Odd;
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
piece_square_t find_index_to_knight(register piece_square_t ps, register uint64_t piecemask) {
	ps.value += get_index2_knight(ps.square);
	if (piecemask & ((1ull << (ps.piece & Piece_Index)) | (1ull << ((ps.piece ^ Piece_Odd) & Piece_Index))))
		++ps.value;
	return ps;
}

static inline
piece_square_t find_index_to_bishop(register piece_square_t ps, register uint64_t piecemask) {
	ps.value += get_index2(ps.square);
	return ps;
}

piece_square_t find_index_error(piece_square_t ps) {
	ps.value = 0;
	return ps;
}

piece_square_t find_index_moved_king(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return (ps2.value & Piece_Index2)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_king(piece_square_t ps, const uint64_t piecemask) {
	if ((ps.square & Square_Rank) != (!(ps.piece & Piece_Black) ? Square_Rank1 : Square_Rank8)) {
		ps.piece |= Piece_Moved;
		return ps;
	}
	return find_index_moved_king(ps, piecemask);
}

piece_square_t find_index_unmoved_pawn(piece_square_t ps, const uint64_t piecemask) {
	ps.piece |= (ps.square & Square_File);
	return (piecemask & (1ull << (ps.value & Piece_Index)))
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_pawn(piece_square_t ps, const uint64_t piecemask) {
	if ((ps.square & Square_Rank) != (!(ps.piece & Piece_Black) ? Square_Rank2 : Square_Rank7)) {
		ps.piece |= Piece_Moved;
		return ps;
	}
	return find_index_unmoved_pawn(ps, piecemask);
}

piece_square_t find_index_knight(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to_knight(ps, piecemask);
	return (piecemask & (1ull << (ps2.value & Piece_Index)))
		|| (piecemask & ((1ull << (ps2.piece & Piece_Index)) | (1ull << ((ps2.piece ^ Piece_Odd) & Piece_Index))))
			? find_index_error(ps2)
			: ps2;
}

piece_square_t find_index_bishop(piece_square_t ps, const uint64_t piecemask) {
	ps = find_index_to_bishop(ps, piecemask);
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

piece_square_t find_index_queen(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_Type)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_moved_pawn(piece_square_t ps, const uint64_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_TypePawn)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_moved_rook(piece_square_t ps, const uint64_t piecemask) {
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
		return find_index_moved_king(ps, piecemask);
	case Piece_Pawn0:
		return find_index_moved_pawn(ps, piecemask);
	case Piece_Knight:
		return find_index_knight(ps, piecemask);
	case Piece_Bishop:
		return find_index_bishop(ps, piecemask);
	case Piece_Rook:
		return find_index_moved_rook(ps, piecemask);
	case Piece_Queen:
		return find_index_queen(ps, piecemask);
	default:
		return find_index_error(ps);
	}
}

static inline
move_t* gen_null(move_t* moves) {
	register piece_square_t nullps = { 0x0800 };
	register move_t move = {
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

static inline
move_t* gen_promo_knight(move_t* moves, register move_t move, register piece_square_t to,
	register const uint64_t piecemask, const uint8_t color)
{
	to.piece = Piece_Knight | color | Piece_Moved;
	move.prim.to = find_index_to_knight(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_bishop(move_t* moves, register move_t move, register piece_square_t to,
	register uint64_t piecemask, const uint8_t color)
{
	to.piece = Piece_Bishop | color | Piece_Moved;
	move.prim.to = find_index_to_bishop(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_rook(move_t* moves, register move_t move, register piece_square_t to,
	register const uint64_t piecemask, const uint8_t color)
{
	to.piece = Piece_Rook | color | Piece_Moved;
	move.prim.to = find_index_to(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_queen(move_t* moves, register move_t move, register piece_square_t to,
	register const uint64_t piecemask, const uint8_t color)
{
	to.piece = Piece_Queen | color | Piece_Moved;
	move.prim.to = find_index_to(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to,
	register const uint64_t piecemask, const uint8_t promo, const uint8_t color)
{
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo_knight(moves, move, to, piecemask, color);
		moves = gen_promo_bishop(moves, move, to, piecemask, color);
		moves = gen_promo_rook(moves, move, to, piecemask, color);
		moves = gen_promo_queen(moves, move, to, piecemask, color);
		return moves;
	}
	*moves++ = move;
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
		if (!(from.piece & Piece_Moved)
			&& !get_square(to.square += vector)) {
				move.prim.to = to;
				move.sec.to.value = from2.value | Piece_EP | 0x0800;
				*moves++ = move;
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
move_t* gen_vector_ep(move_t* moves,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = {
		.square = state.ep.square & ~Square_FileInvalid
	};
	register piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = get_square(from.square)) & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color)) {
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
move_t* gen_vector_king(move_t* moves, register piece_square_t from,
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
move_t* gen_vector_knight(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(from2.square = to.square)) & color)) {
			register move_t move = {
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
move_t* gen_ep_white(move_t* moves, register const uint64_t piecemask) {
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
move_t* gen_pawn_black(move_t* moves, register piece_square_t from, register const uint64_t piecemask) {
	moves = gen_vector_pawn(moves, from, piecemask, Vec_SW, Square_Rank1, Piece_Black, Piece_White);
	moves = gen_vector_pawn(moves, from, piecemask, Vec_SE, Square_Rank1, Piece_Black, Piece_White);
	return gen_push_pawn(moves, from, piecemask, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves, register const uint64_t piecemask) {
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
move_t* gen_king(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
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
move_t* gen_knight(move_t* moves, register piece_square_t from,
	const uint8_t color)
{
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
bool check_knights(register square_t square, uint64_t* mask, piece_t* piece,
	const uint8_t color)
{
	for (; *piece < (Piece_Knight + Count_Knights + (color & Piece_Black)); *piece = find_next(mask)) {
		if (check_knight(get_piece(*piece), square)) {
			return true;
		}
	}
	return false;
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
bool check_sliders(register square_t square, uint64_t mask, piece_t piece,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	return (dir_mask)
		&& (check_bishops(square, &mask, &piece, dir_mask, color)
		|| check_rooks(square, &mask, &piece, dir_mask, color)
		|| check_queens(square, &mask, &piece, dir_mask, color));
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
bool check_pieces(register square_t square, register const uint64_t piecemask,
	register const dir_mask_t dir_mask, const uint8_t color)
{
	piece_t piece = Piece_Knight + (color & Piece_Black) + get_index2(square);
	const uint8_t piece2 = piece ^ Piece_Odd;
	uint64_t mask = piecemask & (0xFFFF0000ull << (color & Piece_Black))
		& ~((1ull << piece2) | (1ull << (piece2 + 1))
		| (1ull << (piece2 + (Piece_Bishop - Piece_Knight)))
		| (1ull << (piece2 + (Piece_Bishop - Piece_Knight) + 1)));
	piece = find_next(&mask);
	return check_knights(square, &mask, &piece, color)
		|| check_sliders(square, mask, piece, dir_mask, color);
}

static inline
bool check_to_white(register square_t square, register const uint64_t piecemask) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_white(square, &dir_mask)
		|| check_pieces(square, piecemask, dir_mask, Piece_White);
}

static inline
bool check_to_black(register square_t square, register const uint64_t piecemask) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_black(square, &dir_mask)
		|| check_pieces(square, piecemask, dir_mask, Piece_Black);
}

static inline
move_t* gen_white(move_t* moves, register const uint64_t piecemask) {
	uint64_t mask = piecemask & 0x00000000FFFFFF00;
	piece_t piece = find_next(&mask);
	moves = gen_kings(moves, Piece_White);
	moves = gen_pawns_white(moves, piecemask, &mask, &piece);
	moves = gen_pieces(moves, &mask, &piece, Piece_White);
	moves = gen_ep_white(moves, piecemask);
#if !NDEBUG
	moves = gen_null(moves);
#endif
	return moves;
}

static inline
move_t* gen_black(move_t* moves, register const uint64_t piecemask) {
	uint64_t mask = piecemask & 0xFFFFFF0000000000;
	piece_t piece = find_next(&mask);
	moves = gen_ep_black(moves, piecemask);
	moves = gen_kings(moves, Piece_Black);
	moves = gen_pawns_black(moves, piecemask, &mask, &piece);
	moves = gen_pieces(moves, &mask, &piece, Piece_Black);
#if !NDEBUG
	moves = gen_null(moves);
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
move_t* gen(move_t* moves, register const uint64_t piecemask) {
	return color == Piece_White
		? gen_white(moves, piecemask)
		: gen_black(moves, piecemask);
}

static inline
bool check(register const uint64_t piecemask) {
	return color == Piece_White
		? check_white(piecemask)
		: check_black(piecemask);
}

static inline
uint64_t clear_prim_from(register piece_square_t from, register uint64_t piecemask) {
	clear_square(from);
	return clear_piece(from, piecemask);
}

static inline
uint64_t set_prim_from(register piece_square_t from, register uint64_t piecemask) {
	set_square(from);
	return set_piece(from, piecemask);
}

static inline
uint64_t clear_prim_to(register piece_square_t to, register uint64_t piecemask) {
	clear_square(to);
	return clear_piece(to, piecemask);
}

static inline
uint64_t set_prim_to(register piece_square_t to, register uint64_t piecemask) {
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
uint64_t clear_ep(register uint64_t piecemask) {
	return piecemask &= ~(1ull << Piece_EP);
}

static inline
uint64_t move_make(register move_t move, register uint64_t piecemask) {
	piecemask = clear_ep(piecemask);

	piecemask = clear_sec(move.sec.from, piecemask);
	piecemask = set_sec(move.sec.to, piecemask);
	piecemask = clear_prim_from(move.prim.from, piecemask);
	piecemask = set_prim_to(move.prim.to, piecemask);

	color ^= Piece_Color;

	state.ep.square = move.sec.to.square;

	return piecemask;
}

static inline
uint64_t move_unmake(register move_t move, register uint64_t piecemask) {
	color ^= Piece_Color;

	piecemask = clear_prim_to(move.prim.to, piecemask);
	piecemask = set_prim_from(move.prim.from, piecemask);
	piecemask = clear_sec(move.sec.to, piecemask);
	piecemask = set_sec(move.sec.from, piecemask);

	return piecemask;
}

uint64_t perft_opt(move_t* moves, const register uint64_t piecemask, register uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	pEnd = gen(moves, piecemask);
	--depth;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		register state_t state2 = state;
		register uint64_t piecemask2 = move_make(*pCurr, piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(piecemask2)) {
			count += depth
				? perft_opt(pEnd, piecemask2, depth)
				: 1;
		}
		move_unmake(*pCurr, piecemask2);
		state = state2;
	}
	return count;
}

uint64_t perft(move_t* moves, uint64_t piecemask, uint8_t depth) {
	return depth
		? perft_opt(moves, piecemask, depth)
		: 1;
}

char get_piece_char(piece_t piece) {
	return piece_chars[(piece & Piece_Index) >> Shift_Type];
}

uint64_t set_pieces_unmoved(uint64_t piecemask) {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(ps.square)) && !(ps.piece & Piece_Moved)) {
				if (!(ps2 = find_index(ps, piecemask)).value) {
					fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
					return 0;
				}
				set_square(ps2);
				piecemask = set_piece(ps2, piecemask);
			}
		}
	}
	return piecemask;
}

uint64_t set_pieces_moved(uint64_t piecemask) {
	piece_square_t ps, ps2;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(ps.square)) & Piece_Moved) {
				if (!(ps2 = find_index_moved(ps, piecemask)).value) {
					fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
					return 0;
				}
				set_square(ps2);
				piecemask = set_piece(ps2, piecemask);
			}
		}
	}
	return piecemask;
}

uint64_t set_pieces(uint64_t piecemask) {
	return (piecemask = set_pieces_unmoved(piecemask))
		&& (piecemask = set_pieces_moved(piecemask))
			? piecemask
			: 0;
}

char* board_write(char* str) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << Shift_Rank;
		*str++ = rank + '1';
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

char buffer[1024];
move_t moves[1024];

int main(int argc, const char* argv[]) {
	uint8_t max = 255;

	if (argc > 1
		&& (!sscanf(argv[1], "%hhu", &max) || !max)) {
			printf("Usage: perft <depth>\n");
			return -1;
	}

	uint64_t piecemask = board_init();
	if (!(piecemask = set_pieces(piecemask))) {
		return 1;
	}

	board_write(buffer);
	printf("%s\n", buffer);

	for (uint8_t depth = 0; depth <= max; ++depth) {
		uint64_t count = perft(moves, piecemask, depth);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return 0;
}
