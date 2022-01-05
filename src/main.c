// bumchess
// Branchless Unmake/Make Chess Move Generator
// 
// Copyright (c) 2022 Dmitry Shechtman
// All rights reserved.

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

enum Type {
	Type_None,
	Type_King,
	Type_Pawn,
	Type_Knight = 4,
	Type_Bishop,
	Type_Rook,
	Type_Queen,
	Type_Count
};

enum Shift {
	Shift_Castling    =  1,
	Shift_Rank        =  4,
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

enum Count {
	Count_Colors    =   2,
	Count_Castlings =   4,
	Count_Type4     =  16,

	Count_Ranks     =   8,
	Count_Files     =   8,
	Count_Squares   = 128,
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

typedef struct {
	square_t ep;
	square_t king;
} state_t;

enum Char {
	Char_Zero = '0',
	Char_Nine = '9',

	Char_Rank = '1',
	Char_File = 'a',
};

piece_t squares[Count_Squares];
piece_t color;
state_t state;

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

move_t* gen_promo_pawn(move_t* moves, move_t move, piece_square_t to, uint8_t promo) {
	if ((to.square & Square_Rank) == promo) {
		move.prim.to.piece = Piece_Queen | color | Piece_Moved;
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
			}
		};
		moves = gen_promo_pawn(moves, move, to, promo);
		if (!(from.piece & Piece_Moved)
			&& !get_square(to.square += vector)) {
				move.prim.to.value = to.value | 0x0800;
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
				}
			};
			moves = gen_promo_pawn(moves, move, to, promo);
	}
	return moves;
}

move_t* gen_vector_ep(move_t* moves, vector_t vector) {
	piece_square_t to = {
		.square = state.ep
	};
	piece_square_t from = to;
	if (!((from.square += vector) & Square_Invalid)
		&& ((to.piece = from.piece = get_square(from.square)) & (Piece_Type | Piece_Color)) == (Piece_Pawn | color)) {
			move_t move = {
				.prim = {
					.from = from,
					.to = to
				},
				.sec = {
					.from = {
						.piece = Piece_Pawn | (color ^ Piece_Color) | Piece_Moved,
						.square = to.square ^ Square_Rank2
					},
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
				}
			};
			*moves++ = move;
	}
	return moves;
}

bool check_vector_leaper(square_t square, piece_t type, vector_t vector) {
	return !((square += vector) & Square_Invalid)
		&& (get_square(square) & (Piece_Type | Piece_Color)) == (type | color);
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
				}
			};
			*moves++ = move;
	}
	return moves;
}

bool check_vector_slider(square_t square, piece_t type, vector_t vector) {
	piece_t piece = 0;
	while (!((square += vector) & Square_Invalid)
		&& !(piece = get_square(square)));
	return (piece & (type | Piece_Color)) == (type | color);
}

move_t* gen_leaper(move_t* moves, piece_square_t from, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		moves = gen_vector_leaper(moves, from, vectors[i]);
	}
	return moves;
}

bool check_leaper(square_t square, piece_t type, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		if (check_vector_leaper(square, type, vectors[i])) {
			return true;
		}
	}
	return false;
}

move_t* gen_slider(move_t* moves, piece_square_t from, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		moves = gen_vector_slider(moves, from, vectors[i]);
	}
	return moves;
}

bool check_slider(square_t square, piece_t type, uint8_t start, uint8_t end) {
	for (uint8_t i = start; i < end; ++i) {
		if (check_vector_slider(square, type, vectors[i])) {
			return true;
		}
	}
	return false;
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

bool check_pawn_white(square_t square) {
	return check_vector_leaper(square, Piece_Pawn, Vec_SW)
		|| check_vector_leaper(square, Piece_Pawn, Vec_SE);
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

bool check_pawn_black(square_t square) {
	return check_vector_leaper(square, Piece_Pawn, Vec_NW)
		|| check_vector_leaper(square, Piece_Pawn, Vec_NE);
}

move_t* gen_pawn(move_t* moves, piece_square_t from) {
	return color == Piece_White
		? gen_pawn_white(moves, from)
		: gen_pawn_black(moves, from);
}

bool check_pawn(square_t square) {
	return color == Piece_White
		? check_pawn_white(square)
		: check_pawn_black(square);
}

move_t* gen_ep(move_t* moves) {
	return !(state.ep & Square_FileInvalid)
		? color == Piece_White
			? gen_ep_white(moves)
			: gen_ep_black(moves)
		: moves;
}

move_t* gen_king(move_t* moves, piece_square_t from) {
	return gen_leaper(moves, from, 0, 8);
}

bool check_king(square_t square) {
	return check_leaper(square, Type_King, 0, 8);
}

move_t* gen_knight(move_t* moves, piece_square_t from) {
	return gen_leaper(moves, from, 8, 16);
}

bool check_knight(square_t square) {
	return check_leaper(square, Type_Knight, 8, 16);
}

move_t* gen_bishop(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 0, 4);
}

bool check_bishop(square_t square) {
	return check_slider(square, Piece_Bishop, 0, 4);
}

move_t* gen_rook(move_t* moves, piece_square_t from) {
	return gen_slider(moves, from, 4, 8);
}

bool check_rook(square_t square) {
	return check_slider(square, Piece_Rook, 4, 8);
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
		state.king = from.square;
		return gen_king(moves, from);
	}
}

bool check_to(square_t square) {
	return check_pawn(square)
		|| check_knight(square)
		|| check_king(square)
		|| check_bishop(square)
		|| check_rook(square);
}

move_t* gen(move_t* moves) {
	square_t square;
	piece_t piece;
	for (uint8_t rank = 0; rank < Count_Ranks; ++rank) {
		square = rank << Shift_Rank;
		for (uint8_t file = 0; file < Count_Files; ++file, ++square) {
			piece = get_square(square);
			if (piece & color) {
				piece_square_t from = {
					.piece = piece,
					.square = square
				};
				moves = gen_piece(moves, from);
			}
		}
	}
	return gen_ep(moves);
}

bool check() {
	return check_to(state.king);
}

void clear_prim_from(piece_square_t from) {
	clear_square(from);
}

void set_prim_from(piece_square_t from) {
	set_square(from);
}

void clear_prim_to(piece_square_t to) {
	to.square &= ~Square_Invalid;
	clear_square(to);
}

void set_prim_to(piece_square_t to) {
	to.square &= ~Square_Invalid;
	to.piece |= Piece_Moved;
	set_square(to);
}

void clear_sec(piece_square_t ps) {
	clear_square(ps);
}

void set_sec(piece_square_t ps) {
	set_square(ps);
}

void set_ep(uint8_t file) {
	state.ep = ((state.ep & Square_Rank) ^ Square_Rank) | file;
}

void set_king(piece_square_t ps) {
	state.king = (ps.piece & Piece_Type) == Piece_King
		? ps.square
		: state.king;
}

void move_make(move_t move) {
	clear_sec(move.sec.from);
	clear_prim_from(move.prim.from);
	set_prim_to(move.prim.to);

	set_ep((move.prim.to.square & Square_File)
		| ((move.prim.to.square & Square_FileInvalid) ^ Square_FileInvalid));

	set_king(move.prim.to);

	color ^= Piece_Color;
}

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

char get_piece_char(piece_t piece) {
	return piece_chars[piece & (Piece_Type | Piece_Black)];
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

const char* fen_read_piece_set(const char* str, piece_square_t* ps) {
	char c = *str++;
	uint8_t type4;
	if (!((type4 = find_type4(c)) & (Type_Count - 1))) {
		return fen_read_error(c);
	}
	ps->piece = type4 | get_moved(type4, ps->square);
	if (!(ps->piece & Piece_Black)) {
		ps->piece |= Piece_White;
	}
	set_square(*ps);
	++ps->square;
	return str;
}

const char* fen_read_piece(const char* str, piece_square_t* ps) {
	char c;
	return ((c = *str) > Char_Zero && c <= Char_Zero + Count_Files)
		? fen_read_piece_clear(str, ps)
		: fen_read_piece_set(str, ps);
}

const char* fen_read_file(const char* str, square_t* square) {
	char c;
	if ((c = *str++) < Char_File || c >= Char_File + Count_Files) {
		return fen_read_error(c);
	}
	*square |= (c - Char_File);
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

const char* fen_read_square(const char* str, square_t* square) {
	*square = 0;
	return (str = fen_read_file(str, square)) && (str = fen_read_rank(str, square))
		? str
		: 0;
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

const char* fen_read_color(const char* str) {
	char c = *str++;
	uint8_t i;
	if ((i = find_color(c)) == Count_Colors) {
		return fen_read_error(c);
	}
	color = color_values[i];
	state.ep = color_ranks[i] | Square_FileInvalid;
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

const char* fen_read_castling_chars(const char* str) {
	do {
		if (!(str = fen_read_castling(str))) {
			return 0;
		}
	} while (*str && *str != ' ');
	return str;
}

const char* fen_read_ep_square(const char* str) {
	return fen_read_square(str, &state.ep);
}

const char* fen_read_castlings(const char* str) {
	return *str != '-'
		? fen_read_castling_chars(str)
		: ++str;
}

const char* fen_read_ep(const char* str) {
	return *str != '-'
		? fen_read_ep_square(str)
		: ++str;
}

const char* fen_read(const char* str) {
	if (!(str = fen_read_squares(str))
		|| !(str = fen_read_char(str, ' ')) || !(str = fen_read_color(str))
		|| ((*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_castlings(str))
		|| (*str && (!(str = fen_read_char(str, ' ')) || !(str = fen_read_ep(str)))))))) {
		return 0;
	}
	return str;
}

char* board_write(char* str) {
	square_t square;
	piece_t piece;
	for (int8_t rank = Count_Ranks - 1; rank >= 0; --rank) {
		square = rank << Shift_Rank;
		*str++ = rank + Char_Rank;
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

bool read_args(int argc, const char* argv[], params_t* params) {
	params->min = 0;
	params->max = UINT8_MAX;
	params->fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
	params->result = UINT64_MAX;
	
	switch (argc) {
	case 1:
		return true;
	case 2:
		if (!read_uint8(argv[1], &params->max)) {
			params->fen = argv[1];
		} else {
			params->min = params->max;
		}
		return true;
	case 4:
		if (!read_uint64(argv[3], &params->result)) {
			return false;
		}
	case 3:
		if (!read_uint8(argv[2], &params->max)) {
			return false;
		}
		params->min = params->max;
		params->fen = argv[1];
		return true;
	default:
		return false;
	}
}

int main(int argc, const char* argv[]) {
	params_t params;
	uint64_t count = 0;

	if (!read_args(argc, argv, &params)) {
		printf("Usage: perft [<fen>] [<depth> [<result>]]\n");
		return -1;
	}

	if (!fen_read(params.fen)) {
		return 1;
	}

	board_write(buffer);
	printf("%s\n", buffer);

	for (uint8_t depth = params.min; depth <= params.max; ++depth) {
		count = perft(moves, depth);
		printf("perft(%3d)=%11" PRIu64 "\n", depth, count);
	}
	
	return params.result == UINT64_MAX || count == params.result
		? 0
		: 2;
}
