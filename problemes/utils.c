#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char* au_moins_une(char** l, int n){
    unsigned int* size = malloc((n+1) * sizeof(unsigned int));//size[i] = strlen(l[i]), size[n] = somme des size[i]
    size[n] = 0;
    for (int i = 0; i<n; i++) {
        size[i] = strlen(l[i]);
        size[n] += size[i];
    }

    char* f = malloc(sizeof(char) * (size[n] + n + 2));

    f[0] = '(';
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

char* au_plus_une(char** l, int n){
    unsigned int* size = malloc((n+1) * sizeof(unsigned int));//size[i] = strlen(l[i]), size[n] = somme des size[i]
    size[n] = 0;
    for (int i = 0; i<n; i++) {
        size[i] = strlen(l[i]);
        size[n] += size[i];
    }

    char* f = malloc(sizeof(char) * (n + n*(size[n] + 2*n) + 2));

    f[0] = '(';
    unsigned int index = 0;
    for (int i = 0; i<n; i++) {
        index++;
        f[index] = '(';
        for (int j = 0; j<n; j++) {
            if (i!=j) {
                index++;
                f[index] = '~';
            }
            strcat(f, l[j]);
            index += size[i] + 1;
            f[index] = '&';
        }
        f[index] = ')';
        index++;
        f[index] = '|';
    }

    f[index] = ')';
    f[index+1] = '\0';
    free(size);
    return f;
}

char* et(char* f1, char* f2){
    int t1 = strlen(f1);
    int t2 = strlen(f2);
    char* f = malloc(sizeof(char) * (t1 + t2 + 4));
    // (f1 & f2)\0
    f[0] = '(';
    strcat(f+1, f1);
    f[t1 + 1] = '&';
    strcat(f+2, f2);
    f[t1+t2+2] = ')';
    f[t1+t2+3] = '\0';

    return f;
}