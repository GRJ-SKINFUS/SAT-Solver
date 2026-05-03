#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

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


char* variable(int i, int j) {
    char* var = malloc(sizeof(char) * (4 + size_int(i) + size_int(j)));//4: 'X' '_' '_' '\0' 
    sprintf(var, "X_%d_%d", i, j);
    return var;
}


char* contrainte_une_ligne (int i, int n){
    char** l = malloc(n * sizeof(char*));
    
    for (int j = 0; j<n; j++) {
        l[j] = variable(i, j);
    }
    
    char* auplus = au_plus_une(l, n);
    char* aumoins = au_moins_une(l, n);
    char* res = et(auplus, aumoins);
    free(aumoins);
    free(auplus);

    return res;
}


char* contrainte_toutes_ligne (int n){
    char** l = malloc(n * sizeof(char*));
    
    for (int i = 0; i<n; i++) {
        l[i] = contrainte_une_ligne(i, n);
    }

    char* res = et_liste(l, n);
    return res;
}


char* contrainte_une_colonne (int j, int n){
    char** l = malloc(n * sizeof(char*));
    
    for (int i = 0; i<n; i++) {
        l[i] = variable(i, j);
    }
    
    char* res = au_plus_une(l, n);

    return res;
}


char* contrainte_toutes_colonnes (int n){
    char** l = malloc(n * sizeof(char*));
    
    for (int j = 0; j<n; j++) {
        l[j] = contrainte_une_colonne(j, n);
    }

    char* res = et_liste(l, n);
    return res;
}


char* contrainte_une_diagonale (int i, int n){
    char** l = malloc(n * sizeof(char*));

    int nb = n;
    if (i < n) {
        nb = i + 1;
    } // à continuer, adapter le nombre d'éléments sur la diagonale et les parcourir
    
    for (int j = 0; j<n; j++) {
        l[j] = variable(i, j);
    }
    
    char* res = au_plus_une(l, n);

    return res;
}


int main() {
    printf("%s\n", contrainte_toutes_ligne(4));
    printf("%s\n", contrainte_toutes_colonnes(4));
    return 0;
}