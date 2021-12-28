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

enum Square {
	Square_File    = 0x07,
	Square_Rank    = 0x70,
	Square_Invalid = 0x88
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
	} sec;
} move_t;

piece_t squares[Count_Squares];
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
}

move_t* gen_push_pawn(move_t* moves, piece_square_t from, vector_t vector) {
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
		*moves++ = move;
		if (!(from.piece & Piece_Moved)
			&& !squares[to.square += vector]) {
				move.prim.to = to;
				*moves++ = move;
		}
	}
	return moves;
}

move_t* gen_vector_pawn(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& (from2.piece = squares[from2.square = to.square]) & (color ^ Piece_Color)) {
			if ((from2.piece & Piece_Type) == Piece_King) {
				return 0;
			}
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

move_t* gen_vector_leaper(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2;
	if (!((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			if ((from2.piece & Piece_Type) == Piece_King) {
				return 0;
			}
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

move_t* gen_vector_slider(move_t* moves, piece_square_t from, vector_t vector) {
	piece_square_t to = from;
	piece_square_t from2 = {0};
	while (!from2.piece
		&& !((to.square += vector) & Square_Invalid)
		&& !((from2.piece = squares[from2.square = to.square]) & color)) {
			if ((from2.piece & Piece_Type) == Piece_King) {
				return 0;
			}
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

move_t* gen_leaper(move_t* moves, piece_square_t from, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		if (!(moves = gen_vector_leaper(moves, from, vectors[i]))) {
			return 0;
		}
	}
	return moves;
}

move_t* gen_slider(move_t* moves, piece_square_t from, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		if (!(moves = gen_vector_slider(moves, from, vectors[i]))) {
			return 0;
		}
	}
	return moves;
}

move_t* gen_pawn_white(move_t* moves, piece_square_t from) {
	if (!(moves = gen_vector_pawn(moves, from, Vec_NW))
		|| !(moves = gen_vector_pawn(moves, from, Vec_NE))) {
			return 0;
	}
	return gen_push_pawn(moves, from, Vec_N);
}

move_t* gen_pawn_black(move_t* moves, piece_square_t from) {
	if (!(moves = gen_vector_pawn(moves, from, Vec_SW))
		|| !(moves = gen_vector_pawn(moves, from, Vec_SE))) {
		return 0;
	}
	return gen_push_pawn(moves, from, Vec_S);
}

move_t* gen_pawn(move_t* moves, piece_square_t from) {
	return color == Piece_White
		? gen_pawn_white(moves, from)
		: gen_pawn_black(moves, from);
}

move_t* gen_king(move_t* moves, piece_square_t from) {
	return gen_leaper(moves, from, 0, 8);
}

move_t* gen_knight(move_t* moves, piece_square_t from) {
	return gen_leaper(moves, from, 8, 16);
}

move_t* gen_bishop(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 0, 4);
}

move_t* gen_rook(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 4, 8);
}

move_t* gen_queen(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 0, 8);
}

move_t* gen_piece(move_t* moves, piece_square_t from) {
	switch (from.piece & Piece_Type) {
	case Piece_Pawn:
		return gen_pawn(moves, from);
	case Piece_Knight:
		return gen_knight(moves, from);
	case Piece_Bishop:
		return gen_bishop(moves, from);
	case Piece_Rook:
		return gen_rook(moves, from);
	case Piece_Queen:
		return gen_queen(moves, from);
	default:
		return gen_king(moves, from);
	}
}

move_t* gen(move_t* moves) {
	square_t square;
	piece_t piece;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		square = rank << 4;
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = squares[square];
			if (piece & color) {
				piece_square_t from = {
					.piece = piece,
					.square = square
				};
				if (!(moves = gen_piece(moves, from))) {
					return 0;
				}
			}
		}
	}
	return moves;
}

void clear_prim_from(piece_square_t from) {
	squares[from.square] = 0x00;
}

void set_prim_from(piece_square_t from) {
	squares[from.square] = from.piece;
}

void clear_prim_to(piece_square_t to) {
	squares[to.square] = 0x00;
}

void set_prim_to(piece_square_t to) {
	squares[to.square] = to.piece | Piece_Moved;
}

void clear_sec(piece_square_t ps) {
	squares[ps.square] = 0x00;
}

void set_sec(piece_square_t ps) {
	squares[ps.square] = ps.piece;
}

void move_make(move_t move) {
	clear_sec(move.sec.from);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	color ^= Piece_Color;
}

void move_unmake(move_t move) {
	color ^= Piece_Color;

	clear_prim_to(move.prim.to);
	set_prim_from(move.prim.from);
	set_sec(move.sec.from);
}

uint64_t perft(move_t* moves, uint8_t depth) {
	move_t* pEnd, * pCurr;
	uint64_t count = 0;
	if (!(pEnd = gen(moves)))
		return 0;
	if (!depth)
		return 1;
	for (pCurr = moves; pCurr != pEnd; ++pCurr) {
		move_make(*pCurr);
		count += perft(pEnd, depth - 1);
		move_unmake(*pCurr);
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
