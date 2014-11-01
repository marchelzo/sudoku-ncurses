#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <HsFFI.h>
#include "SudokuCore_stub.h"

typedef enum { RO, RW } SqType;

typedef struct {
    SqType type;
    int val;
} Square;

typedef struct {
    Square *sqs;
    int xPos;
    int yPos;
} Puzzle;

void curses_init(void);
char *nextWord(char**);
Puzzle *parsePuzzle(char*);
void displayPuzzle(Puzzle *p);

/* movement functions */
void moveUp(Puzzle*);
void moveDown(Puzzle*);
void moveLeft(Puzzle*);
void moveRight(Puzzle*);

/* insert a number in to the currently selected square */
void insertNumber(Puzzle*, int);

/* wait for input to manipulate a puzzle */
void pollInput(Puzzle*);

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    char *ps = malloc(162);
    ps = getPuzzle();
    Puzzle *p = parsePuzzle(ps);
    curses_init();
    displayPuzzle(p);
    while (1) {
        pollInput(p);
        displayPuzzle(p);
        refresh();
    }
    endwin();
    hs_exit();
    return 0;
}

void curses_init(void)
{
    initscr();
    nl();
    noecho();
    raw();
    cbreak();
    keypad(stdscr, true);
}

Puzzle *parsePuzzle(char *pString)
{
    Puzzle *p = malloc(sizeof(Puzzle));
    p->sqs = malloc(sizeof(Square) * 81);
    p->xPos = 0;
    p->yPos = 0;

    /* parse the string and update the puzzle's squares accordingly */
    int i = 0;
    while (i < 81 && pString[0] != '\0') {
        char *w = nextWord(&pString);
        SqType t;
        int v;
        if (w[0] == '0') {
            t = RW;
            v = 0;
        } else {
            t = RO;
            v = w[0] - '0';
        }
        p->sqs[i].type = t;
        p->sqs[i].val = v;
        ++i;
    }
    return p;
}

char *nextWord(char **s)
{
    size_t len = strlen(*s);
    size_t wordlen = 0;
    for (int i = 0; (*s)[i] != ' ' && i < len; ++i, ++wordlen);
    if (len == 0) return NULL;
    char *w = malloc(wordlen + 1);
    for (int i = 0; i < wordlen; ++i) {
        w[i] = (*s)[0];
        ++(*s);
    }
    ++(*s);
    w[wordlen] = '\0';
    return w;
}

void displayPuzzle(Puzzle *p)
{
    for (int i = 0; i < 9; ++i) {
        for (int j = 0; j < 9; ++j) {
            char c;
            if (p->sqs[9*i + j].val == 0)
                c = '%';
            else
                c = p->sqs[9*i + j].val + '0';
            char *sep = (j % 3 == 2 && j < 8) ? " | " : " ";
            mvprintw(1 + i + (i/3), 1 + j*2 + (j/3) * 2 , "%c%s", c, sep);
        }
        if (i == 2 || i == 5)
            mvaddstr(1 + i + (i+1) / 3, 1, "---------------------");
    }
    int i = p->yPos;
    int j = p->xPos;
    move(1 + i + (i/3), 1 + j*2 + (j/3) * 2);
}

void pollInput(Puzzle *p)
{
    int pressed = getch();
    switch (pressed) {
    case KEY_UP:
        moveUp(p);
        break;
    case KEY_DOWN:
        moveDown(p);
        break;
    case KEY_RIGHT:
        moveRight(p);
        break;
    case KEY_LEFT:
        moveLeft(p);
        break;
    case 'Q':
        endwin();
        exit(0);
    default:
        if (pressed >= '0' && pressed <= '9') {
            insertNumber(p, pressed - '0');
        }
    }
}

void moveUp(Puzzle *p)
{
    if (p->yPos > 0)
        --p->yPos;
}

void moveDown(Puzzle *p)
{
    if (p->yPos < 8)
        ++p->yPos;
}

void moveRight(Puzzle *p)
{
    if (p->xPos < 8)
        ++p->xPos;
}

void moveLeft(Puzzle *p)
{
    if (p->xPos > 0)
        --p->xPos;
}

void insertNumber(Puzzle *p, int n)
{
    int i = p->yPos * 9 + p->xPos;
    if (p->sqs[i].type == RO)
        return;
    if (n == 0) {
        p->sqs[i].val = 0;
    } else {
        p->sqs[i].val = n;
    }
}
