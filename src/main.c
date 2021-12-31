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
	square_t square;
} ep_state_t;

typedef struct {
	uint64_t piecemask;
	ep_state_t ep;
} state_t;

piece_t squares[Count_Squares];
piece_square_t pieces[Count_Pieces];
state_t state;
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
	state.piecemask = 0;
}

static inline
uint8_t get_index2_knight(register square_t square) {
	return (((square ^ (square >> Shift_Rank)) & 1) ^ 1) * 3;
}

static inline
uint8_t get_index2(register square_t square) {
	return ((square ^ (square >> Shift_Rank)) & 1) << Shift_Odd;
}

static inline
piece_square_t find_index_to(register piece_square_t ps) {
	for (uint64_t mask = state.piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

static inline
piece_square_t find_index_to_knight(register piece_square_t ps) {
	ps.value += get_index2_knight(ps.square);
	return ps;
}

static inline
piece_square_t find_index_to_bishop(register piece_square_t ps) {
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
	return (state.piecemask & (1ull << (ps.value & Piece_Index)))
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
	return (state.piecemask & (1ull << (ps2.value & Piece_Index)))
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
	ps.piece |= (state.piecemask >> (Piece_King | (ps.piece & Piece_Black))) & 1;
	return (state.piecemask & (1ull << (ps.value & Piece_Index)))
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
	const uint8_t color)
{
	to.piece = Piece_Knight | color | Piece_Moved;
	move.prim.to = find_index_to_knight(to);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_bishop(move_t* moves, register move_t move, register piece_square_t to,
	const uint8_t color)
{
	to.piece = Piece_Bishop | color | Piece_Moved;
	move.prim.to = find_index_to_bishop(to);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_rook(move_t* moves, register move_t move, register piece_square_t to,
	const uint8_t color)
{
	to.piece = Piece_Rook | color | Piece_Moved;
	move.prim.to = find_index_to(to);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_queen(move_t* moves, register move_t move, register piece_square_t to,
	const uint8_t color)
{
	to.piece = Piece_Queen | color | Piece_Moved;
	move.prim.to = find_index_to(to);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to,
	const uint8_t promo, const uint8_t color)
{
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo_knight(moves, move, to, color);
		moves = gen_promo_bishop(moves, move, to, color);
		moves = gen_promo_rook(moves, move, to, color);
		moves = gen_promo_queen(moves, move, to, color);
		return moves;
	}
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_push_pawn(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
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
move_t* gen_vector_pawn(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
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
move_t* gen_vector_ep(move_t* moves,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = {
		.square = state.ep.square & ~Square_FileInvalid
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
move_t* gen_vector_king(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
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
move_t* gen_vector_knight(move_t* moves, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
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
bool check_square_knight(register square_t square,
	const uint8_t color)
{
	register piece_t piece = squares[square];
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
	return !((square += vector) & Square_Invalid)
		&& check_square_knight(square, color);
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
bool check_vector_slider(register square_t src, register square_t dest,
	const vector_t vector, const uint8_t dir, const dir_mask_t dir_mask)
{
	if (!(dir_mask & (1 << dir))) {
		return false;
	}
	for (src += vector; !squares[src += vector]; );
	return src == dest;
}

static inline
bool check_vert(register square_t src, register square_t dest, register int8_t drank,
	const dir_mask_t dir_mask)
{
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_N, Dir_N, dir_mask)
		: check_vector_slider(src, dest, Vec_S, Dir_S, dir_mask);
}

static inline
bool check_horiz(register square_t src, register square_t dest, register int8_t dfile,
	const dir_mask_t dir_mask)
{
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_E, Dir_E, dir_mask)
		: check_vector_slider(src, dest, Vec_W, Dir_W, dir_mask);
}

static inline
bool check_diag1(register square_t src, register square_t dest, register int8_t drank,
	const dir_mask_t dir_mask)
{
	return !(drank & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_NE, Dir_NE, dir_mask)
		: check_vector_slider(src, dest, Vec_SW, Dir_SW, dir_mask);
}

static inline
bool check_diag2(register square_t src, register square_t dest, register int8_t dfile,
	const dir_mask_t dir_mask)
{
	return !(dfile & Square_FileInvalid)
		? check_vector_slider(src, dest, Vec_SE, Dir_SE, dir_mask)
		: check_vector_slider(src, dest, Vec_NW, Dir_NW, dir_mask);
}

static inline
bool check_ortho(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank,
	const dir_mask_t dir_mask)
{
	return !dfile
		? check_vert(src, dest, drank, dir_mask)
		: !drank
			? check_horiz(src, dest, dfile, dir_mask)
			: false;
}

static inline
bool check_diag(register square_t src, register square_t dest, register int8_t dfile, register int8_t drank,
	const dir_mask_t dir_mask)
{
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
	if (state.piecemask & (1ull << Piece_EP)) {
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
	if (state.piecemask & (1ull << Piece_EP)) {
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
bool check_king(register piece_square_t from, register square_t src) {
	register square_t dest = from.square;
	register uint8_t delta = abs(dest - src);
	return delta == Vec_E || delta == Vec_NW || delta == Vec_N || delta == Vec_NE;
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
	const dir_mask_t dir_mask)
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
	const dir_mask_t dir_mask)
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
	const dir_mask_t dir_mask)
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
	register piece_t piece = (Piece_King | color) & Piece_Index;
	return gen_king(moves, pieces[piece], color);
}

static inline
bool check_kings(register square_t square,
	const uint8_t color)
{
	register piece_t piece = (Piece_King | color) & Piece_Index;
	return check_king(pieces[piece], square);
}

static inline
move_t* gen_pawns_white(move_t* moves) {
	register piece_t piece = Piece_Pawn0;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Pawns; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_pawn_white(moves, pieces[piece]);
		}
	}
	return moves;
}

static inline
move_t* gen_pawns_black(move_t* moves) {
	register piece_t piece = Piece_Pawn0 | Piece_Black;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Pawns; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_pawn_black(moves, pieces[piece]);
		}
	}
	return moves;
}

static inline
move_t* gen_knights(move_t* moves,
	const uint8_t color)
{
	register piece_t piece = (Piece_Knight | color) & Piece_Index;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Knights; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_knight(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_knights(register square_t square,
	const uint8_t color)
{
	register piece_t piece = ((Piece_Knight + get_index2(square)) | color) & Piece_Index;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Knights2; ++i, ++piece, mask >>= 1) {
		if ((mask & 1) && check_knight(pieces[piece], square)) {
			return true;
		}
	}
	return false;
}

static inline
move_t* gen_bishops(move_t* moves,
	const uint8_t color)
{
	register piece_t piece = (Piece_Bishop | color) & Piece_Index;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Bishops; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_bishop(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_bishops(register square_t square,
	const dir_mask_t dir_mask, const uint8_t color)
{
	if (dir_mask & (DirMask_SW | DirMask_SE | DirMask_NW | DirMask_NE)) {
		register piece_t piece = ((Piece_Bishop + get_index2(square)) | color) & Piece_Index;
		register uint64_t mask = state.piecemask >> piece;
		for (uint8_t i = 0; i < Count_Bishops2; ++i, ++piece, mask >>= 1) {
			if ((mask & 1) && check_bishop(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
move_t* gen_rooks(move_t* moves,
	const uint8_t color)
{
	register piece_t piece = (Piece_Rook | color) & Piece_Index;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_rook(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_rooks(register square_t square,
	const dir_mask_t dir_mask, const uint8_t color)
{
	if (dir_mask & (DirMask_S | DirMask_W | DirMask_E | DirMask_N)) {
		register piece_t piece = (Piece_Rook | color) & Piece_Index;
		register uint64_t mask = state.piecemask >> piece;
		for (uint8_t i = 0; i < Count_Rooks; ++i, ++piece, mask >>= 1) {
			if ((mask & 1) && check_rook(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
move_t* gen_queens(move_t* moves,
	const uint8_t color)
{
	register piece_t piece = (Piece_Queen | color) & Piece_Index;
	register uint64_t mask = state.piecemask >> piece;
	for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask >>= 1) {
		if (mask & 1) {
			moves = gen_queen(moves, pieces[piece], color);
		}
	}
	return moves;
}

static inline
bool check_queens(register square_t square,
	const dir_mask_t dir_mask, const uint8_t color)
{
	if (dir_mask) {
		register piece_t piece = (Piece_Queen | color) & Piece_Index;
		register uint64_t mask = state.piecemask >> piece;
		for (uint8_t i = 0; i < Count_Queens; ++i, ++piece, mask >>= 1) {
			if ((mask & 1) && check_queen(pieces[piece], square, dir_mask)) {
				return true;
			}
		}
	}
	return false;
}

static inline
bool check_to_white(register square_t square) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_white(square, &dir_mask)
		|| check_knights(square, Piece_White)
		|| check_bishops(square, dir_mask, Piece_White)
		|| check_rooks(square, dir_mask, Piece_White)
		|| check_queens(square, dir_mask, Piece_White);
}

static inline
bool check_to_black(register square_t square) {
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
#if !NDEBUG
	moves = gen_null(moves);
#endif
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
#if !NDEBUG
	moves = gen_null(moves);
#endif
	return moves;
}

static inline
bool check_white() {
	piece_t piece = Piece_King | Piece_Black;
	return check_to_white(pieces[piece].square);
}

static inline
bool check_black() {
	piece_t piece = Piece_King;
	return check_to_black(pieces[piece].square);
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
void clear_square(register piece_square_t ps) {
	squares[ps.square] = 0x00;
}

static inline
void set_square(register piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

static inline
void clear_piece(register piece_square_t ps) {
	register piece_t piece = ps.piece & Piece_Index;
	state.piecemask &= ~(1ull << piece);
}

static inline
void set_piece(register piece_square_t ps) {
	register piece_t piece = ps.piece & Piece_Index;
	pieces[piece] = ps;
	state.piecemask |= (1ull << piece);
}

static inline
void clear_prim_from(register piece_square_t from) {
	clear_square(from);
	clear_piece(from);
}

static inline
void set_prim_from(register piece_square_t from) {
	set_square(from);
	set_piece(from);
}

static inline
void clear_prim_to(register piece_square_t to) {
	clear_square(to);
	clear_piece(to);
}

static inline
void set_prim_to(register piece_square_t to) {
	to.piece |= Piece_Moved;
	set_square(to);
	set_piece(to);
}

static inline
void clear_sec(register piece_square_t ps) {
	clear_square(ps);
	clear_piece(ps);
}

static inline
void set_sec(register piece_square_t ps) {
	set_square(ps);
	set_piece(ps);
}

static inline
void clear_ep() {
	state.piecemask &= ~(1ull << Piece_EP);
}

static inline
void move_make(register move_t move) {
	clear_ep();

	clear_sec(move.sec.from);
	set_sec(move.sec.to);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	color ^= Piece_Color;

	state.ep.square = move.sec.to.square;
}

static inline
void move_unmake(register move_t move) {
	color ^= Piece_Color;

	clear_prim_to(move.prim.to);
	set_prim_from(move.prim.from);
	clear_sec(move.sec.to);
	set_sec(move.sec.from);
}

uint64_t perft_opt(move_t* moves, register uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	state_t state2;
	pEnd = gen(moves);
	--depth;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		state2 = state;
		move_make(*pCurr);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check())
			count += depth
				? perft_opt(pEnd, depth)
				: 1;
		move_unmake(*pCurr);
		state = state2;
	}
	return count;
}

uint64_t perft(move_t* moves, register uint8_t depth) {
	return depth
		? perft_opt(moves, depth)
		: 1;
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
