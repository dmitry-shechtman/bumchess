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

enum Count {
	Count_Ranks    =   8,
	Count_Files    =   8,
	Count_Squares  = 128,
};

typedef uint8_t piece_t;
typedef uint8_t square_t;

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

int main() {
	board_init();
	board_write(buffer);
	printf("%s", buffer);
	return 0;
}
