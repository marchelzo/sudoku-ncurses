all: main.c
	gcc -std=c11 -g -o main main.c -lncurses
