#ifndef UTILS
#define UTILS 

#include <stdlib.h>
#include <string.h>

//A TESTER
/*
Entrees : l une liste de n formules atomiques
Sorties : la disjonction des fonctions
*/
char* au_moins_une(char** l, int n);

//A CORRIGER
/*
Entrees : l une liste de n formules atomiques
Sorties : une formule vraie ssi au plus une des formules est vraies
*/
char* au_plus_une(char** l, int n);

/*
Entrees : f1 et f2 deux formules atomiques
Sortie : (f1 & f2)
*/
char* et(char* f1, char* f2);

#endif