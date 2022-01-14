CC = gcc

FLAGS = -std=c11 -pedantic -Wall -Ofast -flto
# FLAGS += -mbmi -DBMI
FLAGS += -DNDEBUG

EXE = main

$(EXE): main.c
	$(CC) $(FLAGS) main.c -o $(EXE)

clean:
	rm -f $(EXE)
