#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <HsFFI.h>
#include "SudokuCore_stub.h"
#include <stdio.h>


#define RED_ON (attron(COLOR_PAIR(1)))
#define BLACK_ON (attron(COLOR_PAIR(2)))
#define GREEN_ON (attron(COLOR_PAIR(3)))

typedef enum { RO, RW } SqType;

typedef struct {
    SqType type;
    int val;
} Square;

typedef struct {
    Square *sqs;
    int xPos;
    int yPos;
    bool solved;
    char *msg;
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

/* determine whether or not a puzzle is in a solved state */
bool isSolved(Puzzle*);

/* helper function for validating puzzles */
bool oneToNine(const int*);

/* debugging function */
void solvedMsg(Puzzle *p);
void rowMessage(Puzzle *p, int);
void colMessage(Puzzle *p, int);

/* free a puzzle object */
void freePuzzle(Puzzle*);

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    char *ps = malloc(162);
    ps = getPuzzle();
    Puzzle *p = parsePuzzle(ps);
    curses_init();
    displayPuzzle(p);
    while (!(p->solved)) {
        pollInput(p);
        displayPuzzle(p);
        refresh();
    }
    p->msg = "You win!";
    displayPuzzle(p);
    refresh();
    getch();
    endwin();
    free(p);
    free(ps);
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
    start_color();
    use_default_colors();
    init_pair(1, COLOR_RED, -1);
    init_pair(2, COLOR_BLACK, -1);
    init_pair(3, COLOR_GREEN, -1);
}

Puzzle *parsePuzzle(char *pString)
{
    Puzzle *p = malloc(sizeof(Puzzle));
    p->sqs = malloc(sizeof(Square) * 81);
    p->xPos = 0;
    p->yPos = 0;
    p->solved = false;

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
                c = ' ';
            else
                c = p->sqs[9*i + j].val + '0';
            char *sep = (j % 3 == 2 && j < 8) ? " | " : " ";
            if (!(p->solved)) {
                if (p->sqs[9*i + j].type == RO)
                    RED_ON;
                else
                    BLACK_ON;
            } else {
                GREEN_ON;
            }
            mvprintw(1 + i + (i/3), 1 + j*2 + (j/3) * 2 , "%c%s", c, sep);
        }
        if (i == 2 || i == 5)
            mvaddstr(1 + i + (i+1) / 3, 1, "----------------------");
    }
    if (p->msg != NULL)
        mvaddstr(15,1,p->msg);
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
    case 'S':
        solvedMsg(p);
        break;
    case 'R':
        {
        int rn = getch() - '0';
        rowMessage(p, rn);
        }
        break;
    case 'C':
        {
        int cn = getch() - '0';
        colMessage(p, cn);
        }
        break;
    default:
        if (pressed >= '0' && pressed <= '9') {
            insertNumber(p, pressed - '0');
            p->solved = isSolved(p);
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

bool isSolved(Puzzle *p)
{
    int *row = calloc(10,sizeof(int));
    int *col = calloc(10,sizeof(int));
    for (int i = 0; i < 9; ++i) {
        int *row = calloc(10,sizeof(int));
        int *col = calloc(10,sizeof(int));
        for (int j = 0; j < 9; ++j) {
            int rowi = 9 * i + j;
            int coli = 9 * j + i;
            ++row[p->sqs[rowi].val];
            ++col[p->sqs[coli].val];
        }
        if (!(oneToNine(row) && oneToNine(col))) {
            return false;
        }
    }
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            int box[10] = {0};
            for (int k = 0; k < 3; ++k) {
                for (int l = 0; l < 3; ++l) {
                    ++box[p->sqs[9*(3*i + k) + 3*j + l].val];
                }
            }
            if (!oneToNine(box))
                return false;
        }
    }
    free(row);
    free(col);
    return true;
}

bool oneToNine(const int *nums)
{
    if (nums[0] > 0)
        return false;
    for (int i = 1; i < 10; ++i)
        if (nums[i] != 1)
            return false;
    return true;
}

void solvedMsg(Puzzle *p)
{
    if (p->msg != NULL)
        free(p->msg);
    char *msg = malloc(1000);
    sprintf(msg, "Solved: %s", p->solved ? "True": "False");
    p->msg = msg;
}

void rowMessage(Puzzle *p, int n)
{
    if (p->msg != NULL)
        free(p->msg);
    p->msg = malloc(1000);
    p->msg[0] = '\0';
    for (int i = 0; i < 9; ++i) {
        char *str = malloc(5);
        sprintf(str, "%d ", p->sqs[9 * n + i].val);
        strcat(p->msg, str);
    }
}

void colMessage(Puzzle *p, int n)
{
    if (p->msg != NULL)
        free(p->msg);
    p->msg = malloc(1000);
    p->msg[0] = '\0';
    for (int i = 0; i < 9; ++i) {
        char *str = malloc(5);
        sprintf(str, "%d ", p->sqs[9 * i + n].val);
        strcat(p->msg, str);
    }
}

void freePuzzle(Puzzle *p)
{
    for (int i = 80; i >= 0; --i) {
        free(p->sqs + i);
    }
    if (p->msg != NULL)
        free(p->msg);
    free(p);
}
