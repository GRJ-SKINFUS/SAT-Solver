#include "utils.h"


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
        strcat(f + j, l[i]);
        j += size[i] + 2;
        f[j-1] = '|';
    }
    f[j-1] = ')';
    f[j] = '\n';
    free(size);
    return f;
}