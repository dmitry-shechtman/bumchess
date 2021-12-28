#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

enum Type {
	Type_None,
	Type_King,
	Type_Pawn   = 3,
	Type_Knight,
	Type_Bishop,
	Type_Rook,
	Type_Queen,
	Type_Count
};

enum Piece {
	Piece_King     = Type_King,
	Piece_Pawn     = Type_Pawn,
	Piece_Knight   = Type_Knight,
	Piece_Bishop   = Type_Bishop,
	Piece_Rook     = Type_Rook,
	Piece_Queen    = Type_Queen,
	Piece_Type     = Piece_Queen,

	Piece_Black    = 0x08,
	Piece_White    = 0x10,
	Piece_Color    = Piece_Black | Piece_White,

	Piece_Moved    = 0x80
};

enum TypeMask {
	TypeMask_King   = 1 << Piece_King,
	TypeMask_Pawn   = 1 << Piece_Pawn,
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
};

typedef uint8_t piece_t;
typedef uint8_t square_t;
typedef int8_t  vector_t;

typedef uint8_t type_mask_t;
typedef uint8_t dir_mask_t;

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
	} sec;
} move_t;

typedef struct {
	square_t ep;
	square_t king;
} state_t;

piece_t squares[Count_Squares];
piece_t color;
state_t state;

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
		squares[i + 0x10] = Piece_Pawn + Piece_White;
		squares[i + 0x60] = Piece_Pawn + Piece_Black;
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
	state.ep = Square_Rank6 | Square_FileInvalid;
}

static inline
move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to, uint8_t promo, uint8_t color) {
	if ((to.square & Square_Rank) == promo) {
		move.prim.to.piece = Piece_Queen | color | Piece_Moved;
	}
	*moves++ = move;
	return moves;
}

static inline
move_t* gen_push_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo, uint8_t color, uint8_t color2) {
	piece_square_t to = from;
	if (!squares[to.square += vector]) {
		move_t move = {
			.prim = {
				.from = from,
				.to = to
			},
			.sec = {
				.from = { 0x0800 },
			}
		};
		moves = gen_promo_pawn(moves, move, to, promo, color);
		if (!(from.piece & Piece_Moved)
			&& !squares[to.square += vector]) {
				move.prim.to.value = to.value | 0x0800;
				*moves++ = move;
		}
	}
	return moves;
}

static inline
move_t* gen_vector_pawn(move_t* moves, piece_square_t from, vector_t vector, uint8_t promo, uint8_t color, uint8_t color2) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& (from2.piece = squares[from2.square = to.square]) & color2) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = from2,
				}
			};
			moves = gen_promo_pawn(moves, move, to, promo, color);
	}
	return moves;
}

static inline
move_t* gen_vector_ep(move_t* moves, vector_t vector, uint8_t color, uint8_t color2) {
	piece_square_t to = {
		.square = state.ep
	};
	piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = squares[from.square]) & (Piece_Type | Piece_Color)) == (Piece_Pawn | color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = {
						.piece = Piece_Pawn | color2 | Piece_Moved,
						.square = state.ep ^ Square_Rank2
					},
				}
		};
		*moves++ = move;
	}
	return moves;
}

static inline
move_t* gen_vector_leaper(move_t* moves, piece_square_t from, vector_t vector, uint8_t color) {
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
				}
			};
			*moves++ = move;
	}
	return moves;
}

static inline
bool check_square_neighbor(square_t square, type_mask_t type_mask, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	piece_t piece = squares[square];
	if (piece & color) {
		return type_mask & (1 << (piece & Piece_Type));
	}
	if (!piece) {
		*dir_mask |= (1 << dir);
	}
	return 0;
}

static inline
bool check_square_knight(square_t square, uint8_t color) {
	piece_t piece = squares[square];
	return (piece & (Piece_Type | Piece_Color)) == (Piece_Knight | color);
}

static inline
bool check_vector(square_t square, type_mask_t type_mask, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return !((square += vector) & Square_Invalid)
		&& check_square_neighbor(square, type_mask, dir, dir_mask, color);
}

static inline
bool check_vector_knight(square_t square, vector_t vector, uint8_t color) {
	return !((square += vector) & Square_Invalid)
		&& check_square_knight(square, color);
}

static inline
bool check_vector_pawn(square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King | TypeMask_Pawn,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_diag(square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Bishop | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
bool check_vector_ortho(square_t square, vector_t vector, uint8_t dir, dir_mask_t* dir_mask, uint8_t color) {
	return check_vector(square, TypeMask_Queen | TypeMask_Rook | TypeMask_King,
		vector, dir, dir_mask, color);
}

static inline
move_t* gen_vector_slider(move_t* moves, piece_square_t from, vector_t vector, uint8_t color) {
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
				}
			};
			*moves++ = move;
	}
	return moves;
}

static inline
bool check_vector_slider(square_t square, piece_t type, vector_t vector, uint8_t color) {
	piece_t piece = 0;
	for (square += vector;
		!((square += vector) & Square_Invalid)
			&& !(piece = squares[square]); );
	return (piece & (type | Piece_Color)) == (type | color);
}

static inline
move_t* gen_pawn_white(move_t* moves, piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_NW, Square_Rank8, Piece_White, Piece_Black);
	moves = gen_vector_pawn(moves, from, Vec_NE, Square_Rank8, Piece_White, Piece_Black);
	return gen_push_pawn(moves, from, Vec_N, Square_Rank8, Piece_White, Piece_Black);
}

static inline
move_t* gen_ep_white(move_t* moves) {
	if (!(state.ep & Square_FileInvalid)) {
		moves = gen_vector_ep(moves, Vec_SW, Piece_White, Piece_Black);
		moves = gen_vector_ep(moves, Vec_SE, Piece_White, Piece_Black);
	}
	return moves;
}

static inline
bool check_neighbors_white_s(square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_pawn(square, Vec_SW, Dir_SW, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_White)
		|| check_vector_pawn (square, Vec_SE, Dir_SE, dir_mask, Piece_White));
}

static inline
bool check_neighbors_white_we(square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_White);
}

static inline
bool check_neighbors_white_n(square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_diag(square, Vec_NW, Dir_NW, dir_mask, Piece_White)
		|| check_vector_ortho(square, Vec_N,  Dir_N,  dir_mask, Piece_White)
		|| check_vector_diag (square, Vec_NE, Dir_NE, dir_mask, Piece_White));
}

static inline
bool check_neighbors_white(square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_white_s(square, dir_mask)
		|| check_neighbors_white_we(square, dir_mask)
		|| check_neighbors_white_n(square, dir_mask);
}

static inline
move_t* gen_pawn_black(move_t* moves, piece_square_t from) {
	moves = gen_vector_pawn(moves, from, Vec_SW, Square_Rank1, Piece_Black, Piece_White);
	moves = gen_vector_pawn(moves, from, Vec_SE, Square_Rank1, Piece_Black, Piece_White);
	return gen_push_pawn(moves, from, Vec_S, Square_Rank1, Piece_Black, Piece_White);
}

static inline
move_t* gen_ep_black(move_t* moves) {
	if (!(state.ep & Square_FileInvalid)) {
		moves = gen_vector_ep(moves, Vec_NW, Piece_Black, Piece_White);
		moves = gen_vector_ep(moves, Vec_NE, Piece_Black, Piece_White);
	}
	return moves;
}

static inline
bool check_neighbors_black_s(square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank)
		&& (check_vector_diag(square, Vec_SW, Dir_SW, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_S,  Dir_S,  dir_mask, Piece_Black)
		|| check_vector_diag (square, Vec_SE, Dir_SE, dir_mask, Piece_Black));
}

static inline
bool check_neighbors_black_we(square_t square, dir_mask_t* dir_mask) {
	return check_vector_ortho(square, Vec_W, Dir_W, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_E, Dir_E, dir_mask, Piece_Black);
}

static inline
bool check_neighbors_black_n(square_t square, dir_mask_t* dir_mask) {
	return (square & Square_Rank) != Square_Rank8
		&& (check_vector_pawn(square, Vec_NW, Dir_NW, dir_mask, Piece_Black)
		|| check_vector_ortho(square, Vec_N, Dir_N, dir_mask, Piece_Black)
		|| check_vector_pawn(square, Vec_NE, Dir_NE, dir_mask, Piece_Black));
}

static inline
bool check_neighbors_black(square_t square, dir_mask_t* dir_mask) {
	return check_neighbors_black_s(square, dir_mask)
		|| check_neighbors_black_we(square, dir_mask)
		|| check_neighbors_black_n(square, dir_mask);
}

static inline
move_t* gen_king(move_t* moves, piece_square_t from, uint8_t color) {
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
move_t* gen_knight(move_t* moves, piece_square_t from, uint8_t color) {
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
bool check_knight(square_t square, uint8_t color) {
	return check_vector_knight(square, Vec_SSW, color)
		|| check_vector_knight(square, Vec_SSE, color)
		|| check_vector_knight(square, Vec_SWW, color)
		|| check_vector_knight(square, Vec_SEE, color)
		|| check_vector_knight(square, Vec_NWW, color)
		|| check_vector_knight(square, Vec_NEE, color)
		|| check_vector_knight(square, Vec_NNW, color)
		|| check_vector_knight(square, Vec_NNE, color);
}

static inline
move_t* gen_bishop(move_t* moves, piece_square_t from, uint8_t color) {
	moves = gen_vector_slider(moves, from, Vec_SW, color);
	moves = gen_vector_slider(moves, from, Vec_SE, color);
	moves = gen_vector_slider(moves, from, Vec_NW, color);
	moves = gen_vector_slider(moves, from, Vec_NE, color);
	return moves;
}

static inline
bool check_bishop(square_t square, dir_mask_t dir_mask, uint8_t color) {
	return (dir_mask & DirMask_SW) && check_vector_slider(square, Piece_Bishop, Vec_SW, color)
		|| (dir_mask & DirMask_SE) && check_vector_slider(square, Piece_Bishop, Vec_SE, color)
		|| (dir_mask & DirMask_NW) && check_vector_slider(square, Piece_Bishop, Vec_NW, color)
		|| (dir_mask & DirMask_NE) && check_vector_slider(square, Piece_Bishop, Vec_NE, color);
}

static inline
move_t* gen_rook(move_t* moves, piece_square_t from, uint8_t color) {
	moves = gen_vector_slider(moves, from, Vec_S, color);
	moves = gen_vector_slider(moves, from, Vec_W, color);
	moves = gen_vector_slider(moves, from, Vec_E, color);
	moves = gen_vector_slider(moves, from, Vec_N, color);
	return moves;
}

static inline
bool check_rook(square_t square, dir_mask_t dir_mask, uint8_t color) {
	return (dir_mask & DirMask_S) && check_vector_slider(square, Piece_Rook, Vec_S, color)
		|| (dir_mask & DirMask_W) && check_vector_slider(square, Piece_Rook, Vec_W, color)
		|| (dir_mask & DirMask_E) && check_vector_slider(square, Piece_Rook, Vec_E, color)
		|| (dir_mask & DirMask_N) && check_vector_slider(square, Piece_Rook, Vec_N, color);
}

static inline
move_t* gen_queen(move_t* moves, piece_square_t from, uint8_t color) {
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
move_t* gen_piece_white(move_t* moves, piece_square_t from) {
	switch (from.piece & Piece_Type) {
	case Piece_Pawn:
		return gen_pawn_white(moves, from);
	case Piece_Knight:
		return gen_knight(moves, from, Piece_White);
	case Piece_Bishop:
		return gen_bishop(moves, from, Piece_White);
	case Piece_Rook:
		return gen_rook(moves, from, Piece_White);
	case Piece_Queen:
		return gen_queen(moves, from, Piece_White);
	default:
		return gen_king(moves, from, Piece_White);
	}
}

static inline
bool check_square_white(square_t square) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_white(square, &dir_mask)
		|| check_knight(square, Piece_White)
		|| check_bishop(square, dir_mask, Piece_White)
		|| check_rook(square, dir_mask, Piece_White);
}

static inline
move_t* gen_piece_black(move_t* moves, piece_square_t from) {
	switch (from.piece & Piece_Type) {
	case Piece_Pawn:
		return gen_pawn_black(moves, from);
	case Piece_Knight:
		return gen_knight(moves, from, Piece_Black);
	case Piece_Bishop:
		return gen_bishop(moves, from, Piece_Black);
	case Piece_Rook:
		return gen_rook(moves, from, Piece_Black);
	case Piece_Queen:
		return gen_queen(moves, from, Piece_Black);
	default:
		return gen_king(moves, from, Piece_Black);
	}
}

static inline
bool check_square_black(square_t square) {
	dir_mask_t dir_mask = 0;
	return check_neighbors_black(square, &dir_mask)
		|| check_knight(square, Piece_Black)
		|| check_bishop(square, dir_mask, Piece_Black)
		|| check_rook(square, dir_mask, Piece_Black);
}

static inline
move_t* gen_white(move_t* moves) {
	square_t square;
	piece_t piece;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		square = rank << 4;
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = squares[square];
			if (piece & Piece_White) {
				piece_square_t from = {
					.piece = piece,
					.square = square
				};
				moves = gen_piece_white(moves, from);
				if ((piece & Piece_Type) == Piece_King) {
					state.king = square;
				}
			}
		}
	}
	return gen_ep_white(moves);
}

static inline
move_t* gen_black(move_t* moves) {
	square_t square;
	piece_t piece;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		square = rank << 4;
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = squares[square];
			if (piece & Piece_Black) {
				piece_square_t from = {
					.piece = piece,
					.square = square
				};
				moves = gen_piece_black(moves, from);
				if ((piece & Piece_Type) == Piece_King) {
					state.king = square;
				}
			}
		}
	}
	return gen_ep_black(moves);
}

static inline
move_t* gen(move_t* moves) {
	return color == Piece_White
		? gen_white(moves)
		: gen_black(moves);
}

static inline
bool check_square(square_t square) {
	return color == Piece_White
		? check_square_white(square)
		: check_square_black(square);
}

static inline
bool check() {
	return check_square(state.king);
}

static inline
void clear_prim_from(piece_square_t from) {
	squares[from.square] = 0x00;
}

static inline
void set_prim_from(piece_square_t from) {
	squares[from.square] = from.piece;
}

static inline
void clear_prim_to(piece_square_t to) {
	squares[to.square & ~Square_Invalid] = 0x00;
}

static inline
void set_prim_to(piece_square_t to) {
	squares[to.square & ~Square_Invalid] = to.piece | Piece_Moved;
}

static inline
void clear_sec(piece_square_t ps) {
	squares[ps.square] = 0x00;
}

static inline
void set_sec(piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

static inline
void set_ep(uint8_t file) {
	state.ep = ((state.ep & Square_Rank) ^ Square_Rank) | file;
}

static inline
void set_king(piece_square_t ps) {
	state.king = (ps.piece & Piece_Type) == Piece_King
		? ps.square
		: state.king;
}

static inline
void move_make(move_t move) {
	clear_sec(move.sec.from);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	set_ep((move.prim.to.square & Square_File)
		| ((move.prim.to.square & Square_FileInvalid) ^ Square_FileInvalid));

	set_king(move.prim.to);

	color ^= Piece_Color;
}

static inline
void move_unmake(move_t move) {
	color ^= Piece_Color;

	clear_prim_to(move.prim.to);
	set_prim_from(move.prim.from);
	set_sec(move.sec.from);
}

uint64_t perft(move_t* moves, uint8_t depth) {
	move_t *pEnd, *pCurr;
	uint64_t count = 0;
	state_t state2;
	if (!depth)
		return 1;
	pEnd = gen(moves);
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		state2 = state;
		move_make(*pCurr);
		if (!check())
			count += perft(pEnd, depth - 1);
		move_unmake(*pCurr);
		state = state2;
	}
	return count;
}

char* board_write(char* str) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << 4;
		*str++ = rank + '1';
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = squares[square];
			*str++ = ' ';
			*str++ = piece
				? piece_chars[piece & (Piece_Type | Piece_Black)]
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
		&& (!sscanf_s(argv[1], "%hhu", &max) || !max)) {
			printf("Usage: perft <depth>\n");
			return -1;
	}

	board_init();
	
	board_write(buffer);
	printf("%s\n", buffer);

	for (uint8_t depth = 0; depth <= max; ++depth) {
		uint64_t count = perft(moves, depth);
		printf("perft(%3d)=%11llu\n", depth, count);
	}
	
	return 0;
}
