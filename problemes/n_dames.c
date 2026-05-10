#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

char* variable (int i, int j) {
    char* var = malloc(sizeof(char) * (4 + size_int(i) + size_int(j)));//4: 'X' '_' '_' '\0' 
    sprintf(var, "X_%d_%d", i, j);
    return var;
}


char* contrainte_une_ligne (int i, int n) {
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


char* contrainte_toutes_ligne (int n) {
    char** l = malloc(n * sizeof(char*));
    
    for (int i = 0; i<n; i++) {
        l[i] = contrainte_une_ligne(i, n);
    }

    char* res = et_liste(l, n);
    return res;
}


char* contrainte_une_colonne (int j, int n) {
    char** l = malloc(n * sizeof(char*));
    
    for (int i = 0; i<n; i++) {
        l[i] = variable(i, j);
    }
    
    char* res = au_plus_une(l, n);

    return res;
}


char* contrainte_toutes_colonnes (int n) {
    char** l = malloc(n * sizeof(char*));
    
    for (int j = 0; j<n; j++) {
        l[j] = contrainte_une_colonne(j, n);
    }

    char* res = et_liste(l, n);
    return res;
}


char* contrainte_une_diagonale_NO_SE (int d, int n) {
    int nb = (d - n + 1) > 0 ? n - (d - n + 1) : n - (n - d - 1);
    char** l = malloc(nb * sizeof(char*));
    
    int iStart = d < n ? n - d - 1 : 0;
    int jStart = d < n ? 0 : d - n + 1;

    for (int i = iStart, j = jStart, k = 0; i < n && j < n; i++, j++, k++) {
        l[k] = variable(i, j);
    }
    
    char* res = au_plus_une(l, nb);

    return res;
}

char* contrainte_une_diagonale_NE_SO (int d, int n) {
    int nb = (d - n + 1) > 0 ? n - (d - n + 1) : n - (n - d - 1);
    char** l = malloc(nb * sizeof(char*));
    
    int iStart = d < n ? n - d - 1 : 0;
    int jStart = d < n ? n-1 : 2*n - d - 2;

    for (int i = iStart, j = jStart, k = 0; i < n && j >= 0; i++, j--, k++) {
        l[k] = variable(i, j);
    }
    
    char* res = au_plus_une(l, nb);

    return res;
}

char* contrainte_toutes_diagonales (int n) {
    char** l = malloc(2 * (2*n - 3) * sizeof(char*));
    const int offset = 2*n - 3;
    
    for (int d = 1; d < 2*n - 2; d++) {
        l[d-1] = contrainte_une_diagonale_NO_SE(d, n);
    }
    for (int d = 1; d < 2*n - 2; d++) {
        l[d-1 + offset] = contrainte_une_diagonale_NE_SO(d, n);
    }

    char* res = et_liste(l, 2*(2*n - 3));
    return res;
}

void gen_formule_n_dames (int n, char* filename) {
    char** l = malloc(3 * sizeof(char*));
    
    l[0] = contrainte_toutes_ligne(n);
    l[1] = contrainte_toutes_colonnes(n);
    l[2] = contrainte_toutes_diagonales(n);

    char* res = et_liste(l, 3);
    write_to_file(res, filename);
}


int main() {
    printf("Génération de la formule pour le problème des n dames...\n");
    gen_formule_n_dames(8, "n_dames_8.txt");
    return 0;
}