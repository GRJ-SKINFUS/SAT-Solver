#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

/* 
Explication du problème :
Il faut attribuer 5 nationalités/ maisons(couleur + position)/boissons/ animaux/ sports à 5 personnes

Nationalités : Anglais / Suédois / Danois / Norvégien / Inconnue
Maisons : 
    Couleur : Rouge / Verte / Jaune / Bleue / Blanche
    Position : 1 / 2 / 3 / 4 / 5 ( 1 = à gauche, 3 = au milieu et 5 = à droite )
Boissons : café / thé / lait / yop / eau
Animaux : chiens / oiseaux / chats / poisson rouge / cheval
Sports : vélo / danse / escalade / basket / karaté

Modélisation :
Critères de validité d'une solution :
-C1- un/e seul/e nationalitée/ maison(couleur + position)/boisson/ animal/ sport par personne
-C2- respecte les 15 phrases (voir sur le DM)

Pour cela une solution est de la forme :
anglais_1 false suedois_1 true .... anglais_4 true suedois_4 false ... , 
où anglais_1 vaut true si l'anglais habite dans la maison 1

On a donc pour chaque maison 5 variables de nationalité, couleurs, boisson, animaux, sports = 20 variables soit 100 variables au final.
(/!\ pas la position) 


*/