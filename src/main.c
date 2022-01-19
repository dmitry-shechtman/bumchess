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
#include <pthread.h>

#ifdef _MSC_VER
#include <intrin.h>
#else
#ifdef BMI
#include <x86intrin.h>
#endif
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

	Shift_Bank        =  4,

	Shift_EP_Index    =  4,
	Shift_Square      =  8,
	Shift_PromoMask   = 16,

	Shift_Moved       =  6,
};

enum Piece {
	Piece_Castling = 0x01,
	Piece_Odd      = 0x02,
	Piece_Index2   = 0x03,
	Piece_Index3   = 0x07,
	Piece_Index4   = 0x0F,

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

	Piece_Guard    = Piece_Black - 1,
	Piece_EP       = Piece_Black,
};

enum TypeMask {
	TypeMask_King   = 0x00000010,
	TypeMask_Pawn   = 0x0000FF00,
	TypeMask_Knight = 0x000F0000,
	TypeMask_Bishop = 0x00F00000,
	TypeMask_Rook   = 0x0F000000,
	TypeMask_Queen  = 0x70000000,
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
	PieceSquare_EP      = Piece_Moved | PieceSquare_Invalid
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
	Count_Colors    =   2,
	Count_Type4     =  16,

	Count_Ranks     =   8,
	Count_Files     =   8,
	Count_Rows      =  16,
	Count_Squares   = 128,

	Count_Banks     =  16,
	Count_Pieces    =  64,
};

enum Max {
	Max_Pawns0    =    4,
	Max_Knights   =    4,
	Max_Bishops   =    4,
	Max_Rooks     =    4,
	Max_Queens    =    3,

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

typedef enum Dir {
	Dir_SW,
	Dir_SE,
	Dir_NW,
	Dir_NE,
	Dir_S,
	Dir_W,
	Dir_E,
	Dir_N
} dir_t;

typedef uint8_t piece_t;
typedef uint8_t square_t;
typedef int8_t  vector_t;

typedef uint64_t piecemask_t;
typedef uint32_t type_mask_t;
typedef uint16_t promo_mask_t;
typedef uint16_t check_t;

typedef union {
	uint16_t value;
	struct {
		piece_t  piece;
		square_t square;
	};
} piece_square_t;

typedef uint64_t row_t;
typedef uint64_t bank_t;

typedef struct {
	piece_square_t from;
	piece_square_t to;
} from_to_t;

typedef struct {
	from_to_t prim;
	from_to_t sec;
} move_t;

enum Char {
	Char_Zero = '0',
	Char_Nine = '9',

	Char_Rank = '1',
	Char_File = 'a',
};

typedef struct {
	union {
		piece_t squares[Count_Squares];
		row_t rows[Count_Rows];
	};
	union {
		piece_square_t pieces[Count_Pieces];
		bank_t banks[Count_Banks];
	};
	piece_t color;
} board_t;

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

typedef uint8_t  depth_t;
typedef uint8_t  pcount_t;
typedef uint64_t ncount_t;

typedef struct {
	const char* fen;
	depth_t  min;
	depth_t  max;
	depth_t  div;
	ncount_t result;
	pcount_t pcount;
} params_t;

typedef uint16_t mcount_t;

typedef struct {
	board_t     board;
	piecemask_t piecemask;
	move_t    moves[Max_Moves];
	ncount_t  count;
	mcount_t  mcount;
	depth_t   depth;
	pthread_t thread;
} pstate_t;

static inline
piece_t find_next(uint32_t* mask) {
#ifdef _MSC_VER
	uint32_t index;
	_BitScanForward(&index, *mask);
#else
	uint8_t index = __builtin_ctz(*mask);
#endif
#ifdef BMI
	*mask = _blsr_u32(*mask);
#else
	*mask &= (*mask - 1);
#endif
	return index;
}

static inline
row_t get_row(const board_t* board, square_t square) {
	return board->rows[square >> Shift_Row];
}

static inline
row_t get_row2(piece_t piece, square_t square) {
	return (row_t)piece << ((square & Square_File) << Shift_File);
}

static inline
piece_t get_square2(register const row_t row, square_t square) {
	return (piece_t)(row >> ((square & Square_File) << Shift_File));
}

static inline
piece_t get_square(const board_t* board, register square_t square) {
	return board->squares[square];
}

static inline
void clear_square(board_t* board, register piece_square_t ps) {
	board->squares[ps.square] = 0x00;
}

static inline
void set_square(board_t* board, register piece_square_t ps) {
	board->squares[ps.square] = ps.piece;
}

static inline
piece_square_t get_piece(bank_t bank, register piece_t piece) {
	register piece_square_t ps = { (uint16_t)(bank >> ((piece & Piece_Index2) << Shift_Bank)) };
	return ps;
}

static inline
bool has_piece(piece_t piece, piecemask_t piecemask) {
	return piecemask & (1ull << (piece & Piece_Index));
}

static inline
void clear_piece(register piece_square_t ps, piecemask_t* piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	*piecemask &= ~(1ull << piece);
}

static inline
void set_piece(board_t* board, register piece_square_t ps, piecemask_t* piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	board->pieces[piece] = ps;
	*piecemask |= (1ull << piece);
}

static inline
bool has_ep(move_t move) {
	return (move.sec.to.piece & Piece_Type4) == Piece_EP;
}

static inline
void set_ep(move_t* move) {
	move->sec.to.piece = Piece_EP;
}

static inline
piece_square_t find_index_to(register piece_square_t ps, register const piecemask_t piecemask) {
	for (piecemask_t mask = piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

static inline
piece_square_t find_index_to16(register piece_square_t ps, register const promo_mask_t pmask) {
	for (promo_mask_t mask = pmask >> (ps.value & Piece_Index4);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

piece_square_t find_index_error(piece_square_t ps) {
	ps.value = 0;
	return ps;
}

piece_square_t find_index_king(piece_square_t ps, const piecemask_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return (ps2.value & Piece_Index2)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_pawn(piece_square_t ps, const piecemask_t piecemask) {
	ps.piece |= (ps.square & Square_File);
	return has_piece(ps.piece, piecemask)
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_rook(piece_square_t ps, const piecemask_t piecemask) {
	ps.piece |= (piecemask >> (Piece_King | (ps.piece & Piece_Black))) & Piece_Castling;
	return has_piece(ps.piece, piecemask)
		? find_index_error(ps)
		: ps;
}

piece_square_t find_index_moved_pawn(piece_square_t ps, const piecemask_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_TypePawn)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index_other(piece_square_t ps, const piecemask_t piecemask) {
	piece_square_t ps2 = find_index_to(ps, piecemask);
	return ((ps.value ^ ps2.value) & Piece_Type)
		? find_index_error(ps2)
		: ps2;
}

piece_square_t find_index(piece_square_t ps, const piecemask_t piecemask) {
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

piece_square_t find_index_moved(piece_square_t ps, const piecemask_t piecemask) {
	switch (ps.piece & Piece_Type) {
	case Piece_King:
		return find_index_king(ps, piecemask);
	case Piece_Pawn0:
		return find_index_moved_pawn(ps, piecemask);
	default:
		return find_index_other(ps, piecemask);
	}
}

static inline
move_t* gen_promo(move_t* moves, register move_t move, register piece_square_t to,
	register const promo_mask_t pmask, const piece_t piece, const uint8_t color)
{
	to.piece = piece | color;
	move.prim.to = find_index_to16(to, pmask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to,
	register const promo_mask_t pmask, const uint8_t promo, const uint8_t color)
{
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo(moves, move, to, pmask, Piece_Knight, color);
		moves = gen_promo(moves, move, to, pmask, Piece_Bishop, color);
		moves = gen_promo(moves, move, to, pmask, Piece_Rook,   color);
		moves = gen_promo(moves, move, to, pmask, Piece_Queen,  color);
		return moves;
	}
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_push2_pawn(move_t* moves, const board_t* board, register const move_t move,
	const uint8_t color2)
{
	register piece_square_t to = move.prim.to;
	register piece_t left = !((to.square - 1) & Square_FileInvalid)
		? get_square(board, to.square - 1)
		: 0;
	register piece_t right = !((to.square + 1) & Square_FileInvalid)
		? get_square(board, to.square + 1)
		: 0;
	if (!get_square(board, to.square)) {
		register move_t move2 = {
			.prim = move.prim,
			.sec = {
				.from = { (((left & Piece_Index3) | ((right & Piece_Index3) << Shift_EP_Index)) << Shift_Square) | PieceSquare_Invalid
					| (((left  & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color2)) << Shift_Moved) },
				.to = { move.sec.from.value | Piece_EP | PieceSquare_Invalid
					| (((right & (Piece_TypePawn | Piece_Color)) == (Piece_Pawn0 | color2)) << Shift_Moved) }
			}
		};
		*moves++ = move2;
	}
	return moves;
}

static inline
move_t* gen_push_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const promo_mask_t pmask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!(from2.piece = get_square(board, from2.square = to.square += vector))) {
		register move_t move = {
			.prim = {
				.from = from,
				.to = to
			},
			.sec = {
				.from = from2,
				.to = { PieceSquare_Invalid }
			}
		};
		moves = gen_promo_pawn(moves, move, to, pmask, promo, color);
		if (!(from.piece & Piece_Moved)) {
			move.prim.to.square += vector;
			moves = gen_push2_pawn(moves, board, move, color2);
		}
	}
	return moves;
}

static inline
move_t* gen_vector_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const promo_mask_t pmask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& ((from2.piece = get_square(board, from2.square = to.square)) & color2)) {
			register move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
					.to = { PieceSquare_Invalid }
				}
			};
			moves = gen_promo_pawn(moves, move, to, pmask, promo, color);
	}
	return moves;
}

static inline
move_t* gen_vector_ep(move_t* moves, register piece_square_t ps, register square_t square, register uint8_t piece,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	if ((ps.value & PieceSquare_EP) == PieceSquare_EP) {
		register piece_square_t to = {
			.piece = piece | Piece_Pawn0 | color | Piece_Moved,
			.square = square
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
				.to = { PieceSquare_Invalid }
			}
		};
		*moves++ = move;
	}
	return moves;
}

static inline
move_t* gen_vector_leaper(move_t* moves, const board_t* board, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(board, from2.square = to.square)) & color)) {
			register move_t move = {
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

static inline
check_t check_vector_knight(const board_t* board, register square_t square,
	const vector_t vector, const uint8_t color)
{
	register piece_t piece;
	return !((square += vector) & Square_Invalid)
		&& ((piece = get_square(board, square)) & (Piece_Type | Piece_Color)) == (Piece_Knight | color)
			? piece | (square << Shift_Square)
			: 0;
}

static inline
move_t* gen_vector_slider(move_t* moves, const board_t* board, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2 = { 0 };
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = get_square(board, from2.square = to.square)) & color)) {
			register move_t move = {
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

static inline
check_t check_vector_slider(const board_t* board, register square_t square,
	const piece_t piece_type, const vector_t vector, const dir_t dir, const uint8_t color)
{
	register piece_t piece = 0;
	while (!((square += vector) & Square_Invalid) && !(piece = get_square(board, square)));
	return (piece & (piece_type | color)) == (piece_type | color)
		? dir | (piece_type | color) | (square << Shift_Square)
		: 0;
}

static inline
check_t check_vector(const board_t* board, register square_t square,
	const type_mask_t type_mask, const piece_t piece_type,
	const vector_t vector, const dir_t dir, const uint8_t color)
{
	register piece_t piece;
	return !((square += vector) & Square_Invalid)
		? (piece = get_square(board, square))
			? (has_piece(piece, (piecemask_t)type_mask << (color & Piece_Black)))
				? piece | (square << Shift_Square)
				: 0
			: check_vector_slider(board, square, piece_type, vector, dir, color)
		: 0;
}

static inline
check_t check_vector_pawn(const board_t* board, register square_t square,
	const vector_t vector, const dir_t dir, const uint8_t color)
{
	return check_vector(board, square,
		TypeMask_Queen | TypeMask_Bishop | TypeMask_King | TypeMask_Pawn,
		Piece_Bishop, vector, dir, color);
}

static inline
check_t check_vector_diag(const board_t* board, register square_t square,
	const vector_t vector, const dir_t dir, const uint8_t color)
{
	return check_vector(board, square,
		TypeMask_Queen | TypeMask_Bishop | TypeMask_King,
		Piece_Bishop, vector, dir, color);
}

static inline
check_t check_vector_ortho(const board_t* board, register square_t square,
	const vector_t vector, const dir_t dir, const uint8_t color)
{
	return check_vector(board, square,
		TypeMask_Queen | TypeMask_Rook | TypeMask_King,
		Piece_Rook, vector, dir, color);
}

static inline
move_t* gen_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const promo_mask_t pmask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	moves = gen_vector_pawn(moves, board, from, pmask, vector + Vec_W, promo, color, color2);
	moves = gen_vector_pawn(moves, board, from, pmask, vector + Vec_E, promo, color, color2);
	return gen_push_pawn(moves, board, from, pmask, vector, promo, color, color2);
}

static inline
move_t* gen_ep(move_t* moves, from_to_t sec,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	register square_t square = sec.to.square & ~Square_FileInvalid;
	moves = gen_vector_ep(moves, sec.from, square,
		sec.from.square & Piece_Index3, vector + Vec_W, color, color2);
	moves = gen_vector_ep(moves, sec.to, square,
		sec.from.square >> Shift_EP_Index, vector + Vec_E, color, color2);
	return moves;
}

static inline
move_t* gen_pawn_white(move_t* moves, const board_t* board,
	register piece_square_t from, register const promo_mask_t pmask)
{
	return gen_pawn(moves, board, from, pmask, Vec_N, Square_Rank8, Piece_White, Piece_Black);
}

static inline
move_t* gen_ep_white(move_t* moves, from_to_t sec) {
	return gen_ep(moves, sec, Vec_S, Piece_White, Piece_Black);
}

static inline
check_t check_square_ss(const board_t* board, register square_t square, const uint8_t color) {
	check_t check;
	return (square & Square_Rank) > Square_Rank2
		&&    ((check = check_vector_knight(board, square, Vec_SSW, color))
			|| (check = check_vector_knight(board, square, Vec_SSE, color)))
				? check
				: 0;
}

static inline
check_t check_square_we(const board_t* board, register square_t square, const uint8_t color) {
	check_t check;
	return (check = check_vector_ortho(board, square, Vec_W, Dir_W, color))
		|| (check = check_vector_ortho(board, square, Vec_E, Dir_E, color))
			? check
			: 0;
}

static inline
check_t check_square_nn(const board_t* board, register square_t square, const uint8_t color) {
	check_t check;
	return (square & Square_Rank) < Square_Rank7
		&&    ((check = check_vector_knight(board, square, Vec_NNW, color))
			|| (check = check_vector_knight(board, square, Vec_NNE, color)))
				? check
				: 0;
}

static inline
check_t check_square_white_s(const board_t* board, register square_t square) {
	check_t check;
	return (square & Square_Rank)
		&&    ((check = check_vector_knight(board, square, Vec_SWW, Piece_White))
			|| (check = check_vector_pawn  (board, square, Vec_SW,  Dir_SW,  Piece_White))
			|| (check = check_vector_ortho (board, square, Vec_S,   Dir_S,   Piece_White))
			|| (check = check_vector_pawn  (board, square, Vec_SE,  Dir_SE,  Piece_White))
			|| (check = check_vector_knight(board, square, Vec_SEE, Piece_White)))
				? check
				: 0;
}

static inline
check_t check_square_white_n(const board_t* board, register square_t square) {
	check_t check;
	return (square & Square_Rank) != Square_Rank8
		&&    ((check = check_vector_knight(board, square, Vec_NWW, Piece_White))
			|| (check = check_vector_diag  (board, square, Vec_NW,  Dir_NW,  Piece_White))
			|| (check = check_vector_ortho (board, square, Vec_N,   Dir_N,   Piece_White))
			|| (check = check_vector_diag  (board, square, Vec_NE,  Dir_NE,  Piece_White))
			|| (check = check_vector_knight(board, square, Vec_NEE, Piece_White)))
				? check
				: 0;
}

static inline
check_t check_square_white(const board_t* board, register square_t square) {
	check_t check;
	return (check = check_square_ss(board, square, Piece_White))
		|| (check = check_square_white_s(board, square))
		|| (check = check_square_we(board, square, Piece_White))
		|| (check = check_square_white_n(board, square))
		|| (check = check_square_nn(board, square, Piece_White))
			? check
			: 0;
}

static inline
move_t* gen_pawn_black(move_t* moves, const board_t* board,
	register piece_square_t from, register const promo_mask_t pmask)
{
	return gen_pawn(moves, board, from, pmask, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves, from_to_t sec) {
	return gen_ep(moves, sec, Vec_N, Piece_Black, Piece_White);
}

static inline
check_t check_square_black_s(const board_t* board, register square_t square) {
	check_t check;
	return (square & Square_Rank)
		&&    ((check = check_vector_knight(board, square, Vec_SWW, Piece_Black))
			|| (check = check_vector_diag  (board, square, Vec_SW,  Dir_SW,  Piece_Black))
			|| (check = check_vector_ortho (board, square, Vec_S,   Dir_S,   Piece_Black))
			|| (check = check_vector_diag  (board, square, Vec_SE,  Dir_SE,  Piece_Black))
			|| (check = check_vector_knight(board, square, Vec_SEE, Piece_Black)))
				? check
				: 0;
}

static inline
check_t check_square_black_n(const board_t* board, register square_t square) {
	check_t check;
	return (square & Square_Rank) != Square_Rank8
		&&    ((check = check_vector_knight(board, square, Vec_NWW, Piece_Black))
			|| (check = check_vector_pawn  (board, square, Vec_NW,  Dir_NW,  Piece_Black))
			|| (check = check_vector_ortho (board, square, Vec_N,   Dir_N,   Piece_Black))
			|| (check = check_vector_pawn  (board, square, Vec_NE,  Dir_NE,  Piece_Black))
			|| (check = check_vector_knight(board, square, Vec_NEE, Piece_Black)))
				? check
				: 0;
}

static inline
check_t check_square_black(const board_t* board, register square_t square) {
	check_t check;
	return (check = check_square_ss(board, square, Piece_Black))
		|| (check = check_square_black_s(board, square))
		|| (check = check_square_we(board, square, Piece_Black))
		|| (check = check_square_black_n(board, square))
		|| (check = check_square_nn(board, square, Piece_Black))
			? check
			: 0;
}

static inline
move_t* gen_king(move_t* moves, const board_t* board, register piece_square_t from, const uint8_t color) {
	moves = gen_vector_leaper(moves, board, from, Vec_SW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_S,  color);
	moves = gen_vector_leaper(moves, board, from, Vec_SE, color);
	moves = gen_vector_leaper(moves, board, from, Vec_W,  color);
	moves = gen_vector_leaper(moves, board, from, Vec_E,  color);
	moves = gen_vector_leaper(moves, board, from, Vec_NW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_N,  color);
	moves = gen_vector_leaper(moves, board, from, Vec_NE, color);
	return moves;
}

static inline
move_t* gen_knight(move_t* moves, const board_t* board, register piece_square_t from, const uint8_t color) {
	moves = gen_vector_leaper(moves, board, from, Vec_SSW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_SSE, color);
	moves = gen_vector_leaper(moves, board, from, Vec_SWW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_SEE, color);
	moves = gen_vector_leaper(moves, board, from, Vec_NWW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_NEE, color);
	moves = gen_vector_leaper(moves, board, from, Vec_NNW, color);
	moves = gen_vector_leaper(moves, board, from, Vec_NNE, color);
	return moves;
}

static inline
move_t* gen_bishop(move_t* moves, const board_t* board, register piece_square_t from, const uint8_t color) {
	moves = gen_vector_slider(moves, board, from, Vec_SW, color);
	moves = gen_vector_slider(moves, board, from, Vec_SE, color);
	moves = gen_vector_slider(moves, board, from, Vec_NW, color);
	moves = gen_vector_slider(moves, board, from, Vec_NE, color);
	return moves;
}

static inline
move_t* gen_rook(move_t* moves, const board_t* board, register piece_square_t from, const uint8_t color) {
	moves = gen_vector_slider(moves, board, from, Vec_S, color);
	moves = gen_vector_slider(moves, board, from, Vec_W, color);
	moves = gen_vector_slider(moves, board, from, Vec_E, color);
	moves = gen_vector_slider(moves, board, from, Vec_N, color);
	return moves;
}

static inline
move_t* gen_queen(move_t* moves, const board_t* board, register piece_square_t from, const uint8_t color) {
	moves = gen_vector_slider(moves, board, from, Vec_SW, color);
	moves = gen_vector_slider(moves, board, from, Vec_S,  color);
	moves = gen_vector_slider(moves, board, from, Vec_SE, color);
	moves = gen_vector_slider(moves, board, from, Vec_W,  color);
	moves = gen_vector_slider(moves, board, from, Vec_E,  color);
	moves = gen_vector_slider(moves, board, from, Vec_NW, color);
	moves = gen_vector_slider(moves, board, from, Vec_N,  color);
	moves = gen_vector_slider(moves, board, from, Vec_NE, color);
	return moves;
}

static inline
move_t* gen_kings(move_t* moves, const board_t* board, bank_t bank,
	const uint8_t color)
{
	return gen_king(moves, board, get_piece(bank, 0), color);
}

static inline
move_t* gen_pawns_white(move_t* moves, const board_t* board, bank_t bank, const uint8_t type,
	type_mask_t* mask, promo_mask_t pmask, piece_t* piece)
{
	for (; *piece < type + Max_Pawns0; *piece = find_next(mask)) {
		moves = gen_pawn_white(moves, board, get_piece(bank, *piece), pmask);
	}
	return moves;
}

static inline
move_t* gen_pawns_black(move_t* moves, const board_t* board, bank_t bank, const uint8_t type,
	type_mask_t* mask, promo_mask_t pmask, piece_t* piece)
{
	for (; *piece < type + Max_Pawns0; *piece = find_next(mask)) {
		moves = gen_pawn_black(moves, board, get_piece(bank, *piece), pmask);
	}
	return moves;
}

static inline
move_t* gen_knights(move_t* moves, const board_t* board, bank_t bank,
	type_mask_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Knight + Max_Knights; *piece = find_next(mask)) {
		moves = gen_knight(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_bishops(move_t* moves, const board_t* board, bank_t bank,
	type_mask_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Bishop + Max_Bishops; *piece = find_next(mask)) {
		moves = gen_bishop(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_rooks(move_t* moves, const board_t* board, bank_t bank,
	type_mask_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Rook + Max_Rooks; *piece = find_next(mask)) {
		moves = gen_rook(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_queens(move_t* moves, const board_t* board, bank_t bank,
	type_mask_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Queen + Max_Queens; *piece = find_next(mask)) {
		moves = gen_queen(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_pieces(move_t* moves, const board_t* board, const bank_t* banks,
	type_mask_t mask, piece_t piece, const uint8_t color)
{
	moves = gen_knights(moves, board, *++banks, &mask, &piece, color);
	moves = gen_bishops(moves, board, *++banks, &mask, &piece, color);
	moves = gen_rooks(moves, board, *++banks, &mask, &piece, color);
	moves = gen_queens(moves, board, *++banks, &mask, &piece, color);
	return moves;
}

static inline
move_t* gen_white_pieces(move_t* moves, const board_t* board, register const piecemask_t piecemask) {
	const bank_t* banks = &board->banks[Type_King];
	type_mask_t mask = piecemask & 0xFFFFFF00;
	promo_mask_t pmask = mask >> Shift_PromoMask;
	piece_t piece = find_next(&mask);
	moves = gen_pawns_white(moves, board, *++banks, Piece_Pawn0, &mask, pmask, &piece);
	moves = gen_pawns_white(moves, board, *++banks, Piece_Pawn1, &mask, pmask, &piece);
	return gen_pieces(moves, board, banks, mask, piece, Piece_White);
}

static inline
move_t* gen_white(move_t* moves, const board_t* board, register const piecemask_t piecemask, from_to_t sec) {
	bank_t kings = board->banks[Type_King];
	moves = gen_kings(moves, board, kings, Piece_White);
	moves = gen_white_pieces(moves, board, piecemask);
	moves = gen_ep_white(moves, sec);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
move_t* gen_black_pieces(move_t* moves, const board_t* board, register const piecemask_t piecemask) {
	const bank_t* banks = &board->banks[Type_King + Type_Count];
	type_mask_t mask = (piecemask >> Piece_Black) & 0xFFFFFF00;
	promo_mask_t pmask = mask >> Shift_PromoMask;
	piece_t piece = find_next(&mask);
	moves = gen_pawns_black(moves, board, *++banks, Piece_Pawn0, &mask, pmask, &piece);
	moves = gen_pawns_black(moves, board, *++banks, Piece_Pawn1, &mask, pmask, &piece);
	return gen_pieces(moves, board, banks, mask, piece, Piece_Black);
}

static inline
move_t* gen_black(move_t* moves, const board_t* board, register const piecemask_t piecemask, from_to_t sec) {
	bank_t kings = board->banks[Type_King + Type_Count];
	moves = gen_kings(moves, board, kings, Piece_Black);
	moves = gen_black_pieces(moves, board, piecemask);
	moves = gen_ep_black(moves, sec);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
check_t check_white(const board_t* board) {
	bank_t bank = board->banks[Type_King + Type_Count];
	return check_square_white(board, get_piece(bank, 0).square);
}

static inline
check_t check_black(const board_t* board) {
	bank_t bank = board->banks[Type_King];
	return check_square_black(board, get_piece(bank, 0).square);
}

static inline
move_t* gen(move_t* moves, const board_t* board, register const piecemask_t piecemask, from_to_t sec) {
	return board->color == Piece_White
		? gen_white(moves, board, piecemask, sec)
		: gen_black(moves, board, piecemask, sec);
}

static inline
check_t check(const board_t* board) {
	return board->color == Piece_White
		? check_white(board)
		: check_black(board);
}

static inline
void clear(board_t* board, register piece_square_t ps, piecemask_t* piecemask) {
	clear_square(board, ps);
	clear_piece(ps, piecemask);
}

static inline
void set(board_t* board, register piece_square_t ps, piecemask_t* piecemask) {
	set_square(board, ps);
	set_piece(board, ps, piecemask);
}

static inline
void set_prim_to(board_t* board, register piece_square_t to, piecemask_t* piecemask) {
	to.piece |= Piece_Moved;
	set_square(board, to);
	set_piece(board, to, piecemask);
}

static inline
void move_make(board_t* board, register move_t move, piecemask_t* piecemask) {
	clear(board, move.sec.from, piecemask);
	set(board, move.sec.to, piecemask);
	clear(board, move.prim.from, piecemask);
	set_prim_to(board, move.prim.to, piecemask);

	board->color ^= Piece_Color;
}

static inline
void move_unmake(board_t* board, register move_t move, piecemask_t* piecemask) {
	board->color ^= Piece_Color;

	clear(board, move.prim.to, piecemask);
	set(board, move.prim.from, piecemask);
	clear(board, move.sec.to, piecemask);
	set(board, move.sec.from, piecemask);
}

ncount_t perft_opt(move_t* moves, board_t* board, register piecemask_t piecemask, from_to_t sec, register depth_t depth);

ncount_t perft_one(move_t* moves, board_t* board, piecemask_t piecemask, depth_t depth, mcount_t mcount) {
	ncount_t count = 0;
	for (mcount_t i = 0; i < mcount; ++i) {
		move_make(board, moves[i], &piecemask);
#if !NDEBUG
		if (moves[i].prim.from.piece)
#endif
		if (!check(board)) {
			count += depth
				? perft_opt(moves + mcount, board, piecemask, moves[i].sec, depth)
				: 1;
		}
		move_unmake(board, moves[i], &piecemask);
	}
	return count;
}

ncount_t perft_opt(move_t* moves, board_t* board, register piecemask_t piecemask, from_to_t sec, register depth_t depth) {
	mcount_t mcount = (mcount_t)(gen(moves, board, piecemask, sec) - moves);
	return perft_one(moves, board, piecemask, depth - 1, mcount);
}

void* perft_start(void* pstate) {
	pstate_t* state = (pstate_t*)pstate;
	state->count = perft_one(state->moves, &state->board, state->piecemask, state->depth, state->mcount);
	return pstate;
}

mcount_t perft_init(move_t* moves, board_t* board, piecemask_t piecemask, from_to_t sec, pcount_t* pcount) {
	mcount_t mcount = (mcount_t)(gen(moves, board, piecemask, sec) - moves);
	*pcount = *pcount > mcount
		? mcount
		: *pcount;
	return mcount;
}

void perft_init_state(pstate_t* state, move_t* moves, mcount_t mcount, board_t* board, piecemask_t piecemask, depth_t depth, pcount_t pindex, pcount_t pcount) {
	mcount_t start = mcount * pindex / pcount;
	mcount_t end = mcount * (pindex + 1) / pcount;
	state->board = *board;
	state->piecemask = piecemask;
	for (mcount_t mindex = start; mindex < end; ++mindex) {
		state->moves[mindex - start] = moves[mindex];
	}
	state->mcount = end - start;
	state->depth = depth;
}

bool perft_run(move_t* moves, mcount_t mcount, board_t* board, piecemask_t piecemask, depth_t depth, pstate_t* states, pcount_t pcount) {
	for (pcount_t i = 0; i < pcount; ++i) {
		perft_init_state(&states[i], moves, mcount, board, piecemask, depth - 1, i, pcount);
		if (pthread_create(&states[i].thread, 0, perft_start, &states[i])) {
			return false;
		}
	}
	return true;
}

ncount_t perft_count(pstate_t* states, pcount_t pcount) {
	ncount_t result = 0;
	for (pcount_t i = 0; i < pcount; ++i) {
		if (pthread_join(states[i].thread, 0)) {
			return 0;
		}
		result += states[i].count;
	}
	return result;
}

ncount_t perft_dyn(move_t* moves, board_t* board, piecemask_t piecemask, from_to_t sec, depth_t depth, pcount_t pcount) {
	mcount_t mcount = perft_init(moves, board, piecemask, sec, &pcount);
	pstate_t* states;
	ncount_t result = 0;
	if ((states = malloc(pcount * sizeof(pstate_t)))) {
		if (perft_run(moves, mcount, board, piecemask, depth, states, pcount)) {
			result = perft_count(states, pcount);
		}
		free(states);
	}
	return result;
}

ncount_t perft_do(move_t* moves, board_t* board, piecemask_t piecemask, from_to_t sec, depth_t depth, pcount_t pcount) {
	return pcount <= 1
		? perft_opt(moves, board, piecemask, sec, depth)
		: perft_dyn(moves, board, piecemask, sec, depth, pcount);
}

ncount_t perft(move_t* moves, board_t* board, piecemask_t piecemask, from_to_t sec, depth_t depth, depth_t div, char* buffer, char* str, pcount_t pcount);
char* move_write(char* str, move_t move);

ncount_t perft_divide(move_t* moves, board_t* board, piecemask_t piecemask, move_t move, depth_t depth, depth_t div, char* buffer, char* str, pcount_t pcount) {
	str = move_write(str, move);
	*str++ = ' ';
	ncount_t count = perft(moves, board, piecemask, move.sec, depth, div, buffer, str, pcount);
	*str = 0;
	printf("%s%11" PRIu64 "\n", buffer, count);
	return count;
}

ncount_t perft(move_t* moves, board_t* board, piecemask_t piecemask, from_to_t sec, depth_t depth, depth_t div, char* buffer, char* str, pcount_t pcount) {
	ncount_t count = 0;
	if (!depth)
		return 1;
	if (!div)
		return perft_do(moves, board, piecemask, sec, depth, pcount);
	mcount_t mcount = (mcount_t)(gen(moves, board, piecemask, sec) - moves);
	--depth;
	--div;
	for (mcount_t i = 0; i < mcount; ++i) {
		move_make(board, moves[i], &piecemask);
#if !NDEBUG
		if (moves[i].prim.from.piece)
#endif
		if (!check(board))
			count += perft_divide(moves + mcount, board, piecemask, moves[i], depth, div, buffer, str, pcount);
		move_unmake(board, moves[i], &piecemask);
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

const char* fen_read_piece_clear(const char* str, board_t* board, piece_square_t* ps) {
	char c = *str++;
	for (uint8_t i = 0; i < c - Char_Zero; ++i, ++ps->square) {
		clear_square(board, *ps);
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

const char* fen_read_piece_set(const char* str, board_t* board, piece_square_t* ps) {
	char c = *str++;
	uint8_t type4;
	if (!((type4 = find_type4(c)) & (Type_Count - 1))) {
		return fen_read_error(c);
	}
	ps->piece = (type4 << Shift_Type) | get_moved(type4, ps->square);
	if (!(ps->piece & Piece_Black)) {
		ps->piece |= Piece_White;
	}
	set_square(board, *ps);
	++ps->square;
	return str;
}

char* fen_write_piece_set(char* str, piece_t piece, uint8_t* count) {
	str = fen_write_piece_clear(str, count);
	*str++ = get_piece_char(piece);
	return str;
}

const char* fen_read_piece(const char* str, board_t* board, piece_square_t* ps) {
	char c;
	return ((c = *str) > Char_Zero && c <= Char_Zero + Count_Files)
		? fen_read_piece_clear(str, board, ps)
		: fen_read_piece_set(str, board, ps);
}

char* fen_write_piece(char* str, const board_t* board, square_t square, uint8_t* count) {
	piece_t piece;
	if ((piece = get_square(board, square))) {
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

const char* fen_read_squares(const char* str, board_t* board) {
	piece_square_t ps;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		for (ps.square = rank << Shift_Rank; !(ps.square & Square_FileInvalid); ) {
			if (!(str = fen_read_piece(str, board, &ps))) {
				return 0;
			}
		}
		if (rank && !(str = fen_read_char(str, '/'))) {
			return 0;
		}
	}
	return str;
}

char* fen_write_squares(char* str, const board_t* board) {
	square_t square;
	uint8_t count = 0;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		for (square = rank << Shift_Rank; !(square & Square_FileInvalid); ++square) {
			str = fen_write_piece(str, board, square, &count);
		}
		str = fen_write_piece_clear(str, &count);
		if (rank) {
			*str++ = '/';
		}
	}
	return str;
}

const char* fen_read_color(const char* str, board_t* board) {
	char c = *str++;
	uint8_t i;
	if ((i = find_color(c)) == Count_Colors) {
		return fen_read_error(c);
	}
	board->color = color_values[i];
	return str;
}

char* fen_write_color(char* str, const board_t* board) {
	*str++ = color_chars[board->color == Piece_Black];
	return str;
}

const char* fen_read_castling(const char* str, board_t* board) {
	char c = *str++;
	uint8_t i;
	piece_square_t ps;
	if ((i = find_castling(c)) == Castling_Count
		|| (ps.piece = get_square(board, ps.square = castling_rooks[i]))
			!= (Piece_Rook | Piece_Moved | color_values[i >> Shift_Castling])
		|| get_square(board, color_kings[i >> Shift_Castling])
			!= (Piece_King | color_values[i >> Shift_Castling])) {
				return fen_read_error(c);
	}
	ps.piece &= ~Piece_Moved;
	set_square(board, ps);
	return str;
}

char* fen_write_castling(char* str, row_t row, uint8_t i) {
	square_t rook_sq = castling_rooks[i];
	square_t king_sq = color_kings[i >> Shift_Castling];
	if ((row & (get_row2(Piece_Type | Piece_Moved, rook_sq) | get_row2(Piece_Type | Piece_Moved, king_sq)))
		== (get_row2(Piece_Rook, rook_sq) | get_row2(Piece_King, king_sq))) {
			*str++ = castling_chars[i];
	}
	return str;
}

char* fen_write_castling_color(char* str, const board_t* board, uint8_t c) {
	row_t row = get_row(board, color_kings[c >> Shift_Castling]);
	str = fen_write_castling(str, row, c | Rook_H);
	str = fen_write_castling(str, row, c | Rook_A);
	return str;
}

const char* fen_read_castling_chars(const char* str, board_t* board) {
	do {
		if (!(str = fen_read_castling(str, board))) {
			return 0;
		}
	} while (*str && *str != ' ');
	return str;
}

char* fen_write_castling_chars(char* str, const board_t* board) {
	str = fen_write_castling_color(str, board, Castling_White);
	str = fen_write_castling_color(str, board, Castling_Black);
	return str;
}

const char* fen_read_ep_square(const char* str, piecemask_t* piecemask, move_t* move) {
	set_ep(move);
	return fen_read_square(str, &move->sec.from.square);
}

char* fen_write_ep_square(char* str, move_t move) {
	return square_write(str, move.sec.to.square);
}

const char* fen_read_castlings(const char* str, board_t* board) {
	return *str != '-'
		? fen_read_castling_chars(str, board)
		: ++str;
}

char* fen_write_castlings(char* str, const board_t* board) {
	char* start = str;
	if ((str = fen_write_castling_chars(str, board)) == start) {
		*str++ = '-';
	}
	return str;
}

const char* fen_read_ep(const char* str, piecemask_t* piecemask, move_t* move) {
	return *str != '-'
		? fen_read_ep_square(str, piecemask, move)
		: ++str;
}

char* fen_write_ep(char* str, move_t move) {
	if (!has_ep(move)) {
		*str++ = '-';
	} else {
		str = fen_write_ep_square(str, move);
	}
	return str;
}

const char* fen_read(const char* str, board_t* board, piecemask_t* piecemask, move_t* move) {
	*move = nullmove;
	*piecemask = (1ull << Piece_Guard) | (1ull << (Piece_Guard + Piece_Black));
	if (!(str = fen_read_squares(str, board))
		|| !(str = fen_read_char(str, ' ')) || !(str = fen_read_color(str, board))
		|| ((*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_castlings(str, board))
			|| (*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_ep(str, piecemask, move)))))))) {
		return 0;
	}
	return str;
}

char* fen_write(char* str, const board_t* board, move_t move) {
	str = fen_write_squares(str, board);
	*str++ = ' ';
	str = fen_write_color(str, board);
	*str++ = ' ';
	str = fen_write_castlings(str, board);
	*str++ = ' ';
	str = fen_write_ep(str, move);
	return str;
}

bool set_piece_unmoved(board_t* board, piece_square_t ps, piecemask_t* piecemask) {
	piece_square_t ps2 = find_index(ps, *piecemask);
	if (!ps2.value) {
		fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
		return false;
	}
	set(board, ps2, piecemask);
	return true;
}

bool set_piece_moved(board_t* board, piece_square_t ps, piecemask_t* piecemask) {
	piece_square_t ps2 = find_index_moved(ps, *piecemask);
	if (!ps2.value) {
		fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
		return false;
	}
	set(board, ps2, piecemask);
	return true;
}

bool set_pieces_unmoved(board_t* board, piecemask_t* piecemask) {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(board, ps.square)) && !(ps.piece & Piece_Moved)
				&& !set_piece_unmoved(board, ps, piecemask)) {
					return false;
			}
		}
	}
	return true;
}

bool set_pieces_moved(board_t* board, piecemask_t* piecemask) {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if (((ps.piece = get_square(board, ps.square)) & Piece_Moved)
				&& !set_piece_moved(board, ps, piecemask)) {
					return false;
			}
		}
	}
	return true;
}

bool set_pieces_ep_get(board_t* board, move_t* move) {
	move->prim.to.square = move->sec.from.square ^ Square_Rank2;
	move->prim.from.square = move->prim.to.square ^ Square_Rank3;
	if (((move->sec.from.square & Square_Rank) != color_ranks[board->color == Piece_Black]
		|| get_square(board, move->prim.from.square)
		|| get_square(board, move->sec.from.square))
		|| (move->prim.to.piece = get_square(board, move->prim.to.square)) != (Piece_Pawn0 | Piece_Moved | (board->color ^ Piece_Color))) {
			fprintf(stderr, "Invalid e.p. square.\n");
			return false;
	}
	return true;
}

bool set_pieces_ep_unmoved(board_t* board, piecemask_t* piecemask, move_t* move) {
	if (has_ep(*move)) {
		if (!set_pieces_ep_get(board, move)) {
			return false;
		}
		move->prim.to.piece &= ~Piece_Moved;
		set_square(board, move->prim.to);
	}
	return true;
}

bool set_pieces_ep_moved(board_t* board, piecemask_t* piecemask, move_t* move) {
	if (has_ep(*move)) {
		move->prim.to.piece = get_square(board, move->prim.to.square) | Piece_Moved;
		clear_square(board, move->prim.to);
		gen_push2_pawn(move, board, *move, board->color);
		set(board, move->prim.to, piecemask);
	}
	return true;
}

bool validate_kings(piecemask_t* piecemask) {
	for (uint8_t i = 0; i < Count_Colors; ++i) {
		piece_t piece = Piece_King | color_values[i];
		if (!has_piece(piece, *piecemask)) {
			fprintf(stderr, "Missing %c.\n", get_piece_char(piece));
			return false;
		}
	}
	return true;
}

bool validate_check(const board_t* board) {
	if (check(board)) {
		fprintf(stderr, "Illegal position.\n");
		return false;
	}
	return true;
}

bool validate(const board_t* board, piecemask_t* piecemask) {
	return validate_kings(piecemask)
		&& validate_check(board);
}

bool set_pieces(board_t* board, piecemask_t* piecemask, move_t* move) {
	return set_pieces_ep_unmoved(board, piecemask, move)
		&& set_pieces_unmoved(board, piecemask)
		&& set_pieces_moved(board, piecemask)
		&& set_pieces_ep_moved(board, piecemask, move)
		&& validate(board, piecemask);
}

char* board_write(char* str, const board_t* board) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << Shift_Rank;
		str = rank_write(str, square);
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = get_square(board, square);
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

bool args_read_pcount(const char* arg, params_t* params) {
	return read_uint8(arg, &params->pcount) && params->pcount;
}

bool args_read_result(const char* arg, params_t* params) {
	return read_uint64(arg, &params->result);
}

bool args_read_flag(const char* arg, params_t* params) {
	switch (arg[1]) {
	case 'd':
		return args_read_div(&arg[2], params);
	case 'p':
		return args_read_pcount(&arg[2], params);
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
	params->pcount = 0;
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

	board_t board;
	params_t params;
	ncount_t count = 0;

	if (!args_read(argc, argv, &params)) {
		printf("Usage: perft [<fen>] [<depth> [<result>]] [-d<divide>] [-p<threads>] [-l]\n");
		return -1;
	}

	move_t move;
	piecemask_t piecemask;
	if (!fen_read(params.fen, &board, &piecemask, &move)
		|| !set_pieces(&board, &piecemask, &move)) {
			return 1;
	}

	if (params.result == UINT64_MAX) {
		board_write(buffer, &board);
		printf("%s\n", buffer);
	} else {
		fen_write(buffer, &board, move);
		printf("\nPosition: %s\n", buffer);
	}

	for (depth_t depth = params.min; depth <= params.max; ++depth) {
		count = perft(moves, &board, piecemask, move.sec, depth, params.div, buffer, buffer, params.pcount);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return params.result == UINT64_MAX || count == params.result
		? 0
		: 2;
}
