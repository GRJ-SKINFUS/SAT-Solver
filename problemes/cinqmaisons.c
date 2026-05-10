#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

#define NUMBER_HOUSE 5
enum {
    NATIONALITE = 1,
    COULEUR = 2,
    BOISSON = 3,
    ANIMAL = 4,
    SPORT = 5,

    ANGLAIS = 1,
    SUEDOIS = 2,
    DANOIS = 3,
    NORVEGIEN = 4,
    ALLEMAND = 5,

    ROUGE = 1,
    VERTE = 2,
    JAUNE = 3,
    BLEUE = 4,
    BLANCHE = 5,

    CAFE = 1,
    THE = 2,
    LAIT = 3,
    YOP = 4,
    EAU = 5,

    CHIEN = 1,
    OISEAU = 2,
    CHAT = 3,
    POISSON = 4,
    CHEVAL = 5,

    VELO = 1,
    DANSE = 2,
    ESCALADE = 3,
    BASKET = 4,
    KARATE = 5,
};


/* 
Explication du problème :
Il faut attribuer 5 nationalités/ maisons(couleur + position)/boissons/ animaux/ sports à 5 personnes

Nationalités : Anglais / Suédois / Danois / Norvégien / Allemand
Maisons : 
    Couleur : Rouge / Verte / Jaune / Bleue / Blanche
    Position : 0 / 1 / 2 / 3 / 4 ( 0 = à gauche, 2 = au milieu et 4 = à droite )
Boissons : café / thé / lait / yop / eau
Animaux : chiens / oiseaux / chats / poisson rouge / cheval
Sports : vélo / danse / escalade / basket / karaté

Modélisation :
Critères de validité d'une solution :
-C1- un/e seul/e nationalité/ maison(couleur + position)/boisson/ animal/ sport par personne
-C2- respecte les 15 phrases (voir sur le DM)

Pour cela une solution est de la forme :
anglais_1 false suedois_1 true .... anglais_4 true suedois_4 false ... , 
où anglais_1 vaut true si l'anglais habite dans la maison 1

On a donc pour chaque maison 5 variables de nationalité, couleurs, boisson, animaux, sports = 25 variables soit 125 variables au final.
(/!\ pas la position) 

En pratique les variables seront nommés : X_c_i_j où :
c est le numéro de la caractéristique ( 1 : nationalité / 2 : couleur / 3 : boisson / 4 : animal / 5 : sport )
i est le numéro de la caractéristique (voir ordre plus haut Ex : couleur 4 = bleu )
j est le numéro de maison
*/

char* variable(int c, int i, int j) {
    char* var = malloc(sizeof(char) * 8);//4: 'X_c_i_j\0'
    sprintf(var, "X_%d_%d_%d", c, i, j);
    return var;
}

//Vérifie que la maison j a une et une seule caractéristique c
char* contrainte_une_caracteristique (int c, int j){
    char** l = malloc(NUMBER_HOUSE * sizeof(char*));
    
    for (int i = 0; i<NUMBER_HOUSE; i++) {
        l[i] = variable(c, i, j);
    }
    
    char* auplus = au_plus_une(l, NUMBER_HOUSE);
    char* aumoins = au_moins_une(l, NUMBER_HOUSE);
    char* res = et(auplus, aumoins);
    free(aumoins);
    free(auplus);

    return res;
}

//Vérifie que deux maisons n'ont pas la même caractéristique c i
char* contrainte_meme_caracteristique (int c, int i){
    char** l = malloc(NUMBER_HOUSE * sizeof(char*));

    for (int j = 0; j<NUMBER_HOUSE; j++) {
        l[j] = variable(c, i, j);
    }

    char* auplus = au_plus_une(l, NUMBER_HOUSE);
    return auplus;
}

//Les 15 contraintes :

//1. L'Anglais vit dans une maison rouge
//2. Le Suédois a des chiens
//3. Le Danois boit du thé
//4. La maison verte est à gauche de la maison blanche
//5. Le propriétaire de la maison verte boit du café
//6. La personne qui fait du vélo a des oiseaux
//7. Le propriétaire de la maison jaune fait de la danse
//8. La personne qui vit dans la maison du centre boit du lait
//9. Le Norvégien habite la première maison
//10. La personne qui fait de l'escalade vit à côté de celle qui a des chats
//11. La personne qui a un cheval est voisine de celle qui fait de la danse
//12. La personne qui fait du basket boit du Yop
//13. L'Allemand fait du karaté
//14. Le Norvégien vit juste à cçoté de la maison bleue
//15. Le fan d'escalade a un voisin qui boit de l'eau

//Vérifie que la caractéristique car1 v1 correspond au moins pour une maison à car2 v2
char* contrainte_correspondance (int car1, int v1, int car2, int v2){
    char** l = malloc(NUMBER_HOUSE * sizeof(char*));

    for (int i = 0; i<NUMBER_HOUSE; i++) {
        l[i] = et(variable(car1, v1, i), variable(car2, v2, i));
    }

    char* res = au_moins_une(l, NUMBER_HOUSE);
    return res;
}

//Vérifie que la maison de caractéristique 1 est à gauche de celle de caractéristique 2
char* contrainte_voisin_gauche (int car1, int v1, int car2, int v2) {
    char** l = malloc((NUMBER_HOUSE - 1) * sizeof(char*));

    for (int i = 0; i<NUMBER_HOUSE-1; i++) {
        l[i] = et(variable(car1, v1, i), variable(car2, v2, i+1));
    }

    char* res = au_moins_une(l, NUMBER_HOUSE-1);
    return res;
}

//Vérifie que la maison de caractéristique 1 est voisine de celle de caractéristique 2
char* contrainte_voisin (int car1, int v1, int car2, int v2) {
    char* c1 = contrainte_voisin_gauche(car1,v1,car2,v2);
    char* c2 = contrainte_voisin_gauche(car2,v2,car1,v1);
    char* res = ou(c1,c2);
    free(c1);free(c2);
    return res;
}

char* contraintes_problemes () {
    char** contraintes = malloc(15 * sizeof(char*));
    contraintes[0] = contrainte_correspondance(NATIONALITE, ANGLAIS, COULEUR, ROUGE);
    contraintes[1] = contrainte_correspondance(NATIONALITE, SUEDOIS, ANIMAL, CHIEN);
    contraintes[2] = contrainte_correspondance(NATIONALITE, DANOIS, BOISSON, THE);
    contraintes[3] = contrainte_voisin_gauche(COULEUR, VERTE, COULEUR, BLANCHE);
    contraintes[4] = contrainte_correspondance(COULEUR, VERTE, BOISSON, CAFE);
    contraintes[5] = contrainte_correspondance(SPORT, VELO, ANIMAL, OISEAU);
    contraintes[6] = contrainte_correspondance(COULEUR, JAUNE, SPORT, DANSE);
    contraintes[7] = variable(BOISSON,LAIT,2);
    contraintes[8] = variable(NATIONALITE, NORVEGIEN, 0);
    contraintes[9] = contrainte_voisin(SPORT, ESCALADE, ANIMAL, CHAT);
    contraintes[10] = contrainte_voisin(ANIMAL, CHEVAL, SPORT, DANSE);
    contraintes[11] = contrainte_correspondance(SPORT, BASKET, BOISSON, YOP);
    contraintes[12] = contrainte_correspondance(NATIONALITE, ALLEMAND, SPORT, KARATE);
    contraintes[13] = contrainte_voisin(NATIONALITE, NORVEGIEN, COULEUR, BLEUE);
    contraintes[14] = contrainte_voisin(SPORT, ESCALADE, BOISSON, EAU);

    char* res = et_liste(contraintes, 15);
    free(contraintes);
    return res;
}

char* gen_formule_maisons () {
    char** contraintes = malloc(sizeof(char*) * (NUMBER_HOUSE*NUMBER_HOUSE*2+1));
    for (int i = 0; i < NUMBER_HOUSE; i++)
    {
        for (int j = 0; j < NUMBER_HOUSE; j++)
        {
            contraintes[i*NUMBER_HOUSE + j] = contrainte_une_caracteristique(i,j);
            contraintes[i*NUMBER_HOUSE + j + NUMBER_HOUSE*NUMBER_HOUSE + 1] = contrainte_meme_caracteristique(i,j);
        }
    }
    contraintes[NUMBER_HOUSE*NUMBER_HOUSE*2] = contraintes_problemes();
    char* res = et_liste(contraintes, (NUMBER_HOUSE*NUMBER_HOUSE*2+1));
    free(contraintes);
    return res;
}

int main() {
    printf("Génération de la formule pour le problème des 5 maisons...\n");
    char* res = gen_formule_maisons();
    write_to_file(res, "cinq_maisons.txt");
    free(res);
    return 0;
}