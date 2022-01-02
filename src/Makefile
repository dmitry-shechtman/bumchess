CC = gcc
FLAGS = -std=c11 -pedantic -Wall -Ofast -flto -DNDEBUG
EXE = main

$(EXE): main.c
	$(CC) $(FLAGS) main.c -o $(EXE)

clean:
	rm -f $(EXE)
