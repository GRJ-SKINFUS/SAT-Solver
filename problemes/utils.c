#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int size_int(int i) {
    if (i == 0) {
        return 1;
    }
    int si = 0;
    int r = i;
    while (r > 0) {
        r = r/10;
        si ++;
    }
    return si;
}

char* au_moins_une(char** l, int n){
    unsigned int* size = malloc((n+1) * sizeof(unsigned int));//size[i] = strlen(l[i]), size[n] = somme des size[i]
    size[n] = 0;
    for (int i = 0; i<n; i++) {
        size[i] = strlen(l[i]);
        size[n] += size[i];
    }

    char* f = malloc(sizeof(char) * (size[n] + n + 2));

    f[0] = '(';
    f[1] = '\0';
    unsigned int j = 1;
    for (int i = 0; i<n; i++) {
        strcat(f, l[i]);
        j += size[i] + 1;
        f[j-1] = '|';
        f[j] = '\0';
    }
    f[j-1] = ')';
    f[j] = '\0';
    free(size);
    return f;
}

char* au_plus_une(char** l, int n) {

    if (n <= 1) { //au plus une formule avec 0 ou 1 formules atomiques est toujours vraie
        char* f = malloc(3);
        strcpy(f, "()");
        return f;
    }

    unsigned int total = 0;

    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {

            total +=
                strlen(l[i]) +
                strlen(l[j]) +
                6;
        }
    }

    char* f = malloc(total + 3);

    f[0] = '\0';

    unsigned int index = 0;

    index += sprintf(f + index, "(");

    int first = 1;

    for (int i = 0; i < n; i++) {

        for (int j = i + 1; j < n; j++) {

            if (!first)
                index += sprintf(f + index, "&");

            first = 0;

            index += sprintf(
                f + index,
                "(~%s|~%s)",
                l[i],
                l[j]
            );
        }
    }

    index += sprintf(f + index, ")");

    return f;
}

char* et(char* f1, char* f2){
    int t1 = strlen(f1);
    int t2 = strlen(f2);
    char* f = malloc(sizeof(char) * (t1 + t2 + 4));
    // (f1 & f2)\0
    sprintf(f, "(%s&%s)", f1, f2);

    return f;
}

char* ou(char* f1, char* f2){
    int t1 = strlen(f1);
    int t2 = strlen(f2);
    char* f = malloc(sizeof(char) * (t1 + t2 + 4));
    // (f1 | f2)\0
    sprintf(f, "(%s|%s)", f1, f2);

    return f;
}

char* et_liste(char** l, int n){
    if (n == 0) {
        char* r = malloc(3);
        strcpy(r, "()");
        return r;
    }

    if (n == 1) {
        char* r = malloc(strlen(l[0]) + 1);
        strcpy(r, l[0]);
        return r;
    }

    char* res = et(l[0], l[1]);

    for (int i = 2; i < n; i++) {
        char* tmp = et(res, l[i]);

        free(res);

        res = tmp;
    }

    return res;
}

void write_to_file (char* formule, char* filename) {
    FILE* f = fopen(filename, "w");
    fprintf(f, "%s", formule);
    fclose(f);
}