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
	Shift_Rank        =  4,

	Shift_Bank        =  4,

	Shift_EP_Index    =  4,
	Shift_Square      =  8,

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
	TypeMask_King   = 1 << Piece_King,
	TypeMask_Pawn   = (1 << Piece_Pawn0) | (1 << Piece_Pawn1),
	TypeMask_Knight = 1 << Piece_Knight,
	TypeMask_Bishop = 1 << Piece_Bishop,
	TypeMask_Rook   = 1 << Piece_Rook,
	TypeMask_Queen  = 1 << Piece_Queen
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
	Count_Castlings =   4,
	Count_Type4     =  16,

	Count_Ranks     =   8,
	Count_Files     =   8,
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

typedef uint8_t piece_t;
typedef uint8_t square_t;
typedef int8_t  vector_t;

typedef uint32_t type_mask_t;

typedef union {
	uint16_t value;
	struct {
		piece_t  piece;
		square_t square;
	};
} piece_square_t;

typedef uint64_t bank_t;

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

typedef struct {
	piece_t squares[Count_Squares];
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

char castling_chars[Count_Castlings] = "KQkq";

square_t castling_rooks[Count_Castlings] = {
	Square_FileH | Square_Rank1, Square_FileA | Square_Rank1,
	Square_FileH | Square_Rank8, Square_FileA | Square_Rank8
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
	uint8_t  pcount;
} params_t;

typedef struct {
	board_t   board;
	uint64_t  piecemask;
	move_t    moves[Max_Moves];
	move_t*   pEnd;
	uint64_t  count;
	uint8_t   depth;
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
uint64_t clear_piece(register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	return piecemask &= ~(1ull << piece);
}

static inline
uint64_t set_piece(board_t* board, register piece_square_t ps, register uint64_t piecemask) {
	register piece_t piece = ps.piece & Piece_Index;
	board->pieces[piece] = ps;
	return piecemask |= (1ull << piece);
}

static inline
piece_square_t find_index_to(register piece_square_t ps, register const uint64_t piecemask) {
	for (uint64_t mask = piecemask >> (ps.value & Piece_Index);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
}

static inline
piece_square_t find_index_to16(register piece_square_t ps, register const uint16_t piecemask) {
	for (uint16_t mask = piecemask >> (ps.value & Piece_Index4);
		mask & 1;
		++ps.value, mask >>= 1);
	return ps;
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

piece_square_t find_index_rook(piece_square_t ps, const uint64_t piecemask) {
	ps.piece |= (piecemask >> (Piece_King | (ps.piece & Piece_Black))) & Piece_Castling;
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
	default:
		return find_index_other(ps, piecemask);
	}
}

static inline
move_t* gen_promo(move_t* moves, register move_t move, register piece_square_t to,
	register const uint16_t piecemask, const piece_t piece, const uint8_t color)
{
	to.piece = piece | color;
	move.prim.to = find_index_to16(to, piecemask);
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to,
	register const uint16_t piecemask, const uint8_t promo, const uint8_t color)
{
	if ((to.square & Square_Rank) == promo) {
		moves = gen_promo(moves, move, to, piecemask, Piece_Knight, color);
		moves = gen_promo(moves, move, to, piecemask, Piece_Bishop, color);
		moves = gen_promo(moves, move, to, piecemask, Piece_Rook,   color);
		moves = gen_promo(moves, move, to, piecemask, Piece_Queen,  color);
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
move_t* gen_push_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const uint16_t piecemask,
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
		moves = gen_promo_pawn(moves, move, to, piecemask, promo, color);
		if (!(from.piece & Piece_Moved)) {
			move.prim.to.square += vector;
			moves = gen_push2_pawn(moves, board, move, color2);
		}
	}
	return moves;
}

static inline
move_t* gen_vector_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const uint16_t piecemask,
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
			moves = gen_promo_pawn(moves, move, to, piecemask, promo, color);
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
bool check_square_knight(const board_t* board, register square_t square,
	const uint8_t color)
{
	register piece_t piece = (square & Square_Invalid) ? 0 : get_square(board, square);
	return (piece & (Piece_Type | Piece_Color)) == (Piece_Knight | color);
}

static inline
bool check_vector(const board_t* board, register square_t square,
	const type_mask_t type_mask, const piece_t piece_type, const vector_t vector, const uint8_t color)
{
	if ((square += vector) & Square_Invalid) {
		return false;
	}
	register piece_t piece = get_square(board, square);
	if (piece) {
		return (piece & color) && (type_mask & (1 << (piece & Piece_Type)));
	}
	while (!((square += vector) & Square_Invalid) && !(piece = get_square(board, square)));
	return (piece & (piece_type | color)) == (piece_type | color);
}

static inline
bool check_vector_knight(const board_t* board, register square_t square,
	const vector_t vector, const uint8_t color)
{
	return check_square_knight(board, square + vector, color);
}

static inline
bool check_vector_pawn(const board_t* board, register square_t square, const vector_t vector, const uint8_t color) {
	return check_vector(board, square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King | TypeMask_Pawn,
		Piece_Bishop, vector, color);
}

static inline
bool check_vector_diag(const board_t* board, register square_t square, const vector_t vector, const uint8_t color) {
	return check_vector(board, square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King,
		Piece_Bishop, vector, color);
}

static inline
bool check_vector_ortho(const board_t* board, register square_t square, const vector_t vector, const uint8_t color) {
	return check_vector(board, square, TypeMask_Queen | TypeMask_Rook | TypeMask_King,
		Piece_Rook, vector, color);
}

static inline
move_t* gen_vector_slider(move_t* moves, const board_t* board, register piece_square_t from,
	const vector_t vector, const uint8_t color)
{
	register piece_square_t to = from;
	register piece_square_t from2 = {0};
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
move_t* gen_pawn(move_t* moves, const board_t* board, register piece_square_t from, register const uint16_t piecemask,
	const vector_t vector, const uint8_t promo, const uint8_t color, const uint8_t color2)
{
	moves = gen_vector_pawn(moves, board, from, piecemask, vector + Vec_W, promo, color, color2);
	moves = gen_vector_pawn(moves, board, from, piecemask, vector + Vec_E, promo, color, color2);
	return gen_push_pawn(moves, board, from, piecemask, vector, promo, color, color2);
}

static inline
move_t* gen_ep(move_t* moves, register const move_t move,
	const vector_t vector, const uint8_t color, const uint8_t color2)
{
	register square_t square = move.sec.to.square & ~Square_FileInvalid;
	moves = gen_vector_ep(moves, move.sec.from, square,
		move.sec.from.square & Piece_Index3, vector + Vec_W, color, color2);
	moves = gen_vector_ep(moves, move.sec.to, square,
		move.sec.from.square >> Shift_EP_Index, vector + Vec_E, color, color2);
	return moves;
}

static inline
move_t* gen_pawn_white(move_t* moves, const board_t* board,
	register piece_square_t from, register const uint16_t piecemask)
{
	return gen_pawn(moves, board, from, piecemask, Vec_N, Square_Rank8, Piece_White, Piece_Black);
}

static inline
move_t* gen_ep_white(move_t* moves, register const move_t move) {
	return gen_ep(moves, move, Vec_S, Piece_White, Piece_Black);
}

static inline
bool check_neighbors_ss(const board_t* board, register square_t square, const uint8_t color) {
	return (square & Square_Rank) > Square_Rank2
		&& (check_vector_knight(board, square, Vec_SSW, color)
		|| check_vector_knight(board, square, Vec_SSE, color));
}

static inline
bool check_neighbors_we(const board_t* board, register square_t square, const uint8_t color) {
	return check_vector_ortho(board, square, Vec_W, color)
		|| check_vector_ortho(board, square, Vec_E, color);
}

static inline
bool check_neighbors_nn(const board_t* board, register square_t square, const uint8_t color) {
	return (square & Square_Rank) < Square_Rank7
		&& (check_vector_knight(board, square, Vec_NNW, color)
		|| check_vector_knight(board, square, Vec_NNE, color));
}

static inline
bool check_neighbors_white_s(const board_t* board, register square_t square) {
	return (square & Square_Rank)
		&&    (check_vector_knight(board, square, Vec_SWW, Piece_White)
			|| check_vector_pawn  (board, square, Vec_SW,  Piece_White)
			|| check_vector_ortho (board, square, Vec_S,   Piece_White)
			|| check_vector_pawn  (board, square, Vec_SE,  Piece_White)
			|| check_vector_knight(board, square, Vec_SEE, Piece_White));
}

static inline
bool check_neighbors_white_n(const board_t* board, register square_t square) {
	return (square & Square_Rank) != Square_Rank8
		&&    (check_vector_knight(board, square, Vec_NWW, Piece_White)
			|| check_vector_diag  (board, square, Vec_NW,  Piece_White)
			|| check_vector_ortho (board, square, Vec_N,   Piece_White)
			|| check_vector_diag  (board, square, Vec_NE,  Piece_White)
			|| check_vector_knight(board, square, Vec_NEE, Piece_White));
}

static inline
bool check_to_white(const board_t* board, register square_t square) {
	return check_neighbors_ss(board, square, Piece_White)
		|| check_neighbors_white_s(board, square)
		|| check_neighbors_we(board, square, Piece_White)
		|| check_neighbors_white_n(board, square)
		|| check_neighbors_nn(board, square, Piece_White);
}

static inline
move_t* gen_pawn_black(move_t* moves, const board_t* board,
	register piece_square_t from, register const uint16_t piecemask)
{
	return gen_pawn(moves, board, from, piecemask, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves, register const move_t move) {
	return gen_ep(moves, move, Vec_N, Piece_Black, Piece_White);
}

static inline
bool check_neighbors_black_s(const board_t* board, register square_t square) {
	return (square & Square_Rank)
		&&    (check_vector_knight(board, square, Vec_SWW, Piece_Black)
			|| check_vector_diag  (board, square, Vec_SW,  Piece_Black)
			|| check_vector_ortho (board, square, Vec_S,   Piece_Black)
			|| check_vector_diag  (board, square, Vec_SE,  Piece_Black)
			|| check_vector_knight(board, square, Vec_SEE, Piece_Black));
}

static inline
bool check_neighbors_black_n(const board_t* board, register square_t square) {
	return (square & Square_Rank) != Square_Rank8
		&&    (check_vector_knight(board, square, Vec_NWW, Piece_Black)
			|| check_vector_pawn  (board, square, Vec_NW,  Piece_Black)
			|| check_vector_ortho (board, square, Vec_N,   Piece_Black)
			|| check_vector_pawn  (board, square, Vec_NE,  Piece_Black)
			|| check_vector_knight(board, square, Vec_NEE, Piece_Black));
}

static inline
bool check_to_black(const board_t* board, register square_t square) {
	return check_neighbors_ss(board, square, Piece_Black)
		|| check_neighbors_black_s(board, square)
		|| check_neighbors_we(board, square, Piece_Black)
		|| check_neighbors_black_n(board, square)
		|| check_neighbors_nn(board, square, Piece_Black);
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
	uint32_t* mask, uint16_t piecemask, piece_t* piece)
{
	for (; *piece < type + Max_Pawns0; *piece = find_next(mask)) {
		moves = gen_pawn_white(moves, board, get_piece(bank, *piece), piecemask);
	}
	return moves;
}

static inline
move_t* gen_pawns_black(move_t* moves, const board_t* board, bank_t bank, const uint8_t type,
	uint32_t* mask, uint16_t piecemask, piece_t* piece)
{
	for (; *piece < type + Max_Pawns0; *piece = find_next(mask)) {
		moves = gen_pawn_black(moves, board, get_piece(bank, *piece), piecemask);
	}
	return moves;
}

static inline
move_t* gen_knights(move_t* moves, const board_t* board, bank_t bank,
	uint32_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Knight + Max_Knights; *piece = find_next(mask)) {
		moves = gen_knight(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_bishops(move_t* moves, const board_t* board, bank_t bank,
	uint32_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Bishop + Max_Bishops; *piece = find_next(mask)) {
		moves = gen_bishop(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_rooks(move_t* moves, const board_t* board, bank_t bank,
	uint32_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Rook + Max_Rooks; *piece = find_next(mask)) {
		moves = gen_rook(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_queens(move_t* moves, const board_t* board, bank_t bank,
	uint32_t* mask, piece_t* piece, const uint8_t color)
{
	for (; *piece < Piece_Queen + Max_Queens; *piece = find_next(mask)) {
		moves = gen_queen(moves, board, get_piece(bank, *piece), color);
	}
	return moves;
}

static inline
move_t* gen_pieces(move_t* moves, const board_t* board, const bank_t* banks,
	uint32_t mask, piece_t piece, const uint8_t color)
{
	moves = gen_knights(moves, board, *++banks, &mask, &piece, color);
	moves = gen_bishops(moves, board, *++banks, &mask, &piece, color);
	moves = gen_rooks(moves, board, *++banks, &mask, &piece, color);
	moves = gen_queens(moves, board, *++banks, &mask, &piece, color);
	return moves;
}

static inline
move_t* gen_white(move_t* moves, const board_t* board, register const uint64_t piecemask, register const move_t move) {
	const bank_t* banks = board->banks;
	uint32_t mask = piecemask & 0xFFFFFF00;
	piece_t piece = find_next(&mask);
	moves = gen_kings(moves, board, *++banks, Piece_White);
	moves = gen_pawns_white(moves, board, *++banks, Piece_Pawn0, &mask, mask >> 16, &piece);
	moves = gen_pawns_white(moves, board, *++banks, Piece_Pawn1, &mask, mask >> 16, &piece);
	moves = gen_pieces(moves, board, banks, mask, piece, Piece_White);
	moves = gen_ep_white(moves, move);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
move_t* gen_black(move_t* moves, const board_t* board, register const uint64_t piecemask, register const move_t move) {
	const bank_t* banks = board->banks + Type_Count;
	uint32_t mask = (piecemask >> Piece_Black) & 0xFFFFFF00;
	piece_t piece = find_next(&mask);
	moves = gen_kings(moves, board, *++banks, Piece_Black);
	moves = gen_pawns_black(moves, board, *++banks, Piece_Pawn0, &mask, mask >> 16, &piece);
	moves = gen_pawns_black(moves, board, *++banks, Piece_Pawn1, &mask, mask >> 16, &piece);
	moves = gen_pieces(moves, board, banks, mask, piece, Piece_Black);
	moves = gen_ep_black(moves, move);
#if !NDEBUG
	*moves++ = nullmove;
#endif
	return moves;
}

static inline
bool check_white(const board_t* board) {
	bank_t bank = board->banks[Type_King + Type_Count];
	return check_to_white(board, get_piece(bank, 0).square);
}

static inline
bool check_black(const board_t* board) {
	bank_t bank = board->banks[Type_King];
	return check_to_black(board, get_piece(bank, 0).square);
}

static inline
move_t* gen(move_t* moves, const board_t* board, register const uint64_t piecemask, register const move_t move) {
	return board->color == Piece_White
		? gen_white(moves, board, piecemask, move)
		: gen_black(moves, board, piecemask, move);
}

static inline
bool check(const board_t* board) {
	return board->color == Piece_White
		? check_white(board)
		: check_black(board);
}

static inline
uint64_t clear(board_t* board, register piece_square_t ps, register uint64_t piecemask) {
	clear_square(board, ps);
	return clear_piece(ps, piecemask);
}

static inline
uint64_t set(board_t* board, register piece_square_t ps, register uint64_t piecemask) {
	set_square(board, ps);
	return set_piece(board, ps, piecemask);
}

static inline
uint64_t set_prim_to(board_t* board, register piece_square_t to, register uint64_t piecemask) {
	to.piece |= Piece_Moved;
	set_square(board, to);
	return set_piece(board, to, piecemask);
}

static inline
uint64_t move_make(board_t* board, register move_t move, register uint64_t piecemask) {
	piecemask = clear(board, move.sec.from, piecemask);
	piecemask = set(board, move.sec.to, piecemask);
	piecemask = clear(board, move.prim.from, piecemask);
	piecemask = set_prim_to(board, move.prim.to, piecemask);

	board->color ^= Piece_Color;

	return piecemask;
}

static inline
uint64_t move_unmake(board_t* board, register move_t move, register uint64_t piecemask) {
	board->color ^= Piece_Color;

	piecemask = clear(board, move.prim.to, piecemask);
	piecemask = set(board, move.prim.from, piecemask);
	piecemask = clear(board, move.sec.to, piecemask);
	piecemask = set(board, move.sec.from, piecemask);

	return piecemask;
}

uint64_t perft_opt(move_t* moves, board_t* board, register uint64_t piecemask, register const move_t move, register uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	pEnd = gen(moves, board, piecemask, move);
	--depth;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		piecemask = move_make(board, *pCurr, piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(board)) {
			count += depth
				? perft_opt(pEnd, board, piecemask, *pCurr, depth)
				: 1;
		}
		piecemask = move_unmake(board, *pCurr, piecemask);
	}
	return count;
}

void* perft_start(void* pstate) {
	pstate_t* state = (pstate_t*)pstate;
	for (move_t* pCurr = state->moves; pCurr != state->pEnd; ++pCurr) {
		state->piecemask = move_make(&state->board, *pCurr, state->piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(&state->board)) {
			state->count += state->depth
				? perft_opt(state->pEnd, &state->board, state->piecemask, *pCurr, state->depth)
				: 1;
		}
		state->piecemask = move_unmake(&state->board, *pCurr, state->piecemask);
	}
	return pstate;
}

uint8_t perft_init(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t* pcount) {
	uint8_t mcount = (uint8_t)(gen(moves, board, piecemask, move) - moves);
	*pcount = *pcount > mcount
		? mcount
		: *pcount;
	return mcount;
}

void perft_init_state(pstate_t* state, move_t* moves, uint8_t mcount, board_t* board, uint64_t piecemask, uint8_t depth, uint8_t pindex, uint8_t pcount) {
	uint8_t start = mcount * pindex / pcount;
	uint8_t end = mcount * (pindex + 1) / pcount;
	state->board = *board;
	state->piecemask = piecemask;
	for (uint8_t mindex = start; mindex < end; ++mindex) {
		state->moves[mindex - start] = moves[mindex];
	}
	state->pEnd = &state->moves[end - start];
	state->count = 0;
	state->depth = depth;
}

bool perft_run(move_t* moves, uint8_t mcount, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, pstate_t* states, uint8_t pcount) {
	for (uint8_t i = 0; i < pcount; ++i) {
		perft_init_state(&states[i], moves, mcount, board, piecemask, depth - 1, i, pcount);
		if (pthread_create(&states[i].thread, 0, perft_start, &states[i])) {
			return false;
		}
	}
	return true;
}

uint64_t perft_count(pstate_t* states, uint8_t pcount) {
	uint64_t result = 0;
	for (uint8_t i = 0; i < pcount; ++i) {
		if (pthread_join(states[i].thread, 0)) {
			return 0;
		}
		result += states[i].count;
	}
	return result;
}

uint64_t perft_dyn(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, uint8_t pcount) {
	uint8_t mcount = perft_init(moves, board, piecemask, move, &pcount);
	pstate_t* states;
	uint64_t result = 0;
	if ((states = malloc(pcount * sizeof(pstate_t)))) {
		if (perft_run(moves, mcount, board, piecemask, move, depth, states, pcount)) {
			result = perft_count(states, pcount);
		}
		free(states);
	}
	return result;
}

uint64_t perft_do(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, uint8_t pcount) {
	return pcount <= 1
		? perft_opt(moves, board, piecemask, move, depth)
		: perft_dyn(moves, board, piecemask, move, depth, pcount);
}

uint64_t perft(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* buffer, char* str, uint8_t pcount);
char* move_write(char* str, move_t move);

uint64_t perft_divide(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* buffer, char* str, uint8_t pcount) {
	str = move_write(str, move);
	*str++ = ' ';
	uint64_t count = perft(moves, board, piecemask, move, depth, div, buffer, str, pcount);
	*str = 0;
	printf("%s%11" PRIu64 "\n", buffer, count);
	return count;
}

uint64_t perft(move_t* moves, board_t* board, uint64_t piecemask, move_t move, uint8_t depth, uint8_t div, char* buffer, char* str, uint8_t pcount) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	if (!depth)
		return 1;
	if (!div)
		return perft_do(moves, board, piecemask, move, depth, pcount);
	pEnd = gen(moves, board, piecemask, move);
	--depth;
	--div;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		piecemask = move_make(board, *pCurr, piecemask);
#if !NDEBUG
		if (pCurr->prim.from.piece)
#endif
		if (!check(board))
			count += perft_divide(pEnd, board, piecemask, *pCurr, depth, div, buffer, str, pcount);
		piecemask = move_unmake(board, *pCurr, piecemask);
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
	if ((i = find_castling(c)) == Count_Castlings
		|| (ps.piece = get_square(board, ps.square = castling_rooks[i])) != (Piece_Rook | Piece_Moved | color_values[i >> Shift_Castling])) {
			return fen_read_error(c);
	}
	ps.piece &= ~Piece_Moved;
	set_square(board, ps);
	return str;
}

char* fen_write_castling(char* str, const board_t* board, uint64_t piecemask, uint8_t i) {
	piece_t color = color_values[i >> Shift_Castling];
	piece_t rook = Piece_Rook | color | ((i & Piece_Castling) ^ Piece_Castling);
	piece_t king = Piece_King | color;
	if ((piecemask & (1ull << rook))
		&& !(board->pieces[rook & Piece_Index].piece & Piece_Moved)
		&& !(board->pieces[king & Piece_Index].piece & Piece_Moved)) {
			*str++ = castling_chars[i];
	}
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

const char* fen_read_ep_square(const char* str, uint64_t* piecemask, move_t* move) {
	*piecemask |= (1ull << Piece_EP);
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

char* fen_write_castlings(char* str, const board_t* board, uint64_t piecemask) {
	char* start = str;
	for (uint8_t i = 0; i < Count_Castlings; ++i) {
		str = fen_write_castling(str, board, piecemask, i);
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

const char* fen_read(const char* str, board_t* board, uint64_t* piecemask, move_t* move) {
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

char* fen_write(char* str, const board_t* board, uint64_t piecemask, move_t move) {
	str = fen_write_squares(str, board);
	*str++ = ' ';
	str = fen_write_color(str, board);
	*str++ = ' ';
	str = fen_write_castlings(str, board, piecemask);
	*str++ = ' ';
	str = fen_write_ep(str, piecemask, move);
	return str;
}

uint64_t set_piece_unmoved(board_t* board, piece_square_t ps, uint64_t piecemask) {
	piece_square_t ps2 = find_index(ps, piecemask);
	if (!ps2.value) {
		fprintf(stderr, "Invalid %c.\n", get_piece_char(ps.piece));
		return 0;
	}
	return set(board, ps2, piecemask);
}

uint64_t set_piece_moved(board_t* board, piece_square_t ps, uint64_t piecemask) {
	piece_square_t ps2 = find_index_moved(ps, piecemask);
	if (!ps2.value) {
		fprintf(stderr, "Too many %c's.\n", get_piece_char(ps.piece));
		return 0;
	}
	return set(board, ps2, piecemask);
}

uint64_t set_pieces_unmoved(board_t* board, uint64_t piecemask) {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if ((ps.piece = get_square(board, ps.square)) && !(ps.piece & Piece_Moved)
				&& !(piecemask = set_piece_unmoved(board, ps, piecemask))) {
					return 0;
			}
		}
	}
	return piecemask;
}

uint64_t set_pieces_moved(board_t* board, uint64_t piecemask) {
	piece_square_t ps;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		ps.square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++ps.square) {
			if (((ps.piece = get_square(board, ps.square)) & Piece_Moved)
				&& !(piecemask = set_piece_moved(board, ps, piecemask))) {
					return 0;
			}
		}
	}
	return piecemask;
}

uint64_t set_pieces_ep_get(board_t* board, uint64_t piecemask, move_t* move) {
	move->prim.to.square = move->sec.from.square ^ Square_Rank2;
	move->prim.from.square = move->prim.to.square ^ Square_Rank3;
	if (((move->sec.from.square & Square_Rank) != color_ranks[board->color == Piece_Black]
		|| get_square(board, move->prim.from.square)
		|| get_square(board, move->sec.from.square))
		|| (move->prim.to.piece = get_square(board, move->prim.to.square)) != (Piece_Pawn0 | Piece_Moved | (board->color ^ Piece_Color))) {
			fprintf(stderr, "Invalid e.p. square.\n");
			return 0;
	}
	return piecemask;
}

uint64_t set_pieces_ep_unmoved(board_t* board, uint64_t piecemask, move_t* move) {
	if (piecemask & (1ull << Piece_EP)) {
		if (!set_pieces_ep_get(board, piecemask, move)) {
			return 0;
		}
		move->prim.to.piece &= ~Piece_Moved;
		set_square(board, move->prim.to);
	}
	return piecemask;
}

uint64_t set_pieces_ep_moved(board_t* board, uint64_t piecemask, move_t* move) {
	if (piecemask & (1ull << Piece_EP)) {
		move->prim.to.piece = get_square(board, move->prim.to.square) | Piece_Moved;
		clear_square(board, move->prim.to);
		gen_push2_pawn(move, board, *move, board->color);
		piecemask = set(board, move->prim.to, piecemask);
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

bool validate_check(const board_t* board) {
	if (check(board)) {
		fprintf(stderr, "Illegal position.\n");
		return false;
	}
	return true;
}

bool validate(const board_t* board, uint64_t piecemask) {
	return validate_kings(piecemask)
		&& validate_check(board);
}

uint64_t set_pieces(board_t* board, uint64_t piecemask, move_t* move) {
	return (piecemask = set_pieces_ep_unmoved(board, piecemask, move))
		&& (piecemask = set_pieces_unmoved(board, piecemask))
		&& (piecemask = set_pieces_moved(board, piecemask))
		&& (piecemask = set_pieces_ep_moved(board, piecemask, move))
		&& validate(board, piecemask)
			? piecemask
			: 0;
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
	uint64_t count = 0;

	if (!args_read(argc, argv, &params)) {
		printf("Usage: perft [<fen>] [<depth> [<result>]] [-d<divide>] [-p<threads>] [-l]\n");
		return -1;
	}

	move_t move;
	uint64_t piecemask;
	if (!fen_read(params.fen, &board, &piecemask, &move)
		|| !(piecemask = set_pieces(&board, piecemask, &move))) {
			return 1;
	}

	if (params.result == UINT64_MAX) {
		board_write(buffer, &board);
		printf("%s\n", buffer);
	} else {
		fen_write(buffer, &board, piecemask, move);
		printf("\nPosition: %s\n", buffer);
	}

	for (uint8_t depth = params.min; depth <= params.max; ++depth) {
		count = perft(moves, &board, piecemask, move, depth, params.div, buffer, buffer, params.pcount);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return params.result == UINT64_MAX || count == params.result
		? 0
		: 2;
}
