all: main.c
	gcc -std=c11 -g -o main main.c -lncurses

main: main.c SudokuCore.o
	ghc --make -no-hs-main -optc -std=c11 -optc-O -o test main.c SudokuCore -lncurses

SudokuCore.o:
	ghc -c -O SudokuCore.hs
