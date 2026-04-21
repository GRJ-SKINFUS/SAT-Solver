#include <stdio.h>
#include <stdlib.h>

int size_int(int i) {
    int si = 0;
    int r = i;
    while (r > 0) {
        r = r/10;
        si ++;
    }
    return si;
}


char* variable(int i, int j) {
    char* var = malloc(sizeof(char) * (4 + size_int(i) + size_int(j)));//4: 'X' '_' '_' '\0' 
    sprintf(var, "X_%d_%d", i, j);
    return var;
}