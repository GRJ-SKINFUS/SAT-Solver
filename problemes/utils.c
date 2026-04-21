#include "utils.h"
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
        strcat(f + j, l[i]);
        j += size[i] + 2;
        f[j-1] = '|';
    }
    f[j-1] = ')';
    f[j] = '\n';
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

    char* f = malloc(sizeof(char) * (n + (n+1)*(size[n] + n + 1) + 2));//FAUX -> manque les ~

    f[0] = '(';
    unsigned int index = 0;
    for (int i = 0; i<n; i++) {
        index ++;
        f[index] = '(';
        for (int j = 0; j<n; j++) {
            if (i!=j) {
                index ++;
                f[index] = '~';
            }
            strcat(f + index, l[j]);
            index += size[i] + 1;
            f[index] = '&';
        }
        f[index-1] = ')';
        f[index] = '|';
    }

    f[index] = ')';
    f[index+1] = '\n';
    free(size);
    return f;
}