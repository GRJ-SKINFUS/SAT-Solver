RAPPORT

Q10 - 
Pour une formule avec n variables et k opérateurs, 
    la fonction effectue des opérations en O(1) pour chaque opérateurs.
    Et pour chaque variable on réalise "union" avec le reste des variables.
    Or union s'effectue en 0(n) pour des listes de taille n.
    Donc list_vars s'effectue en O(k + n**2).

Pour améliorer la complexité on pourrait utiliser des dictionnaires, implémentés par exemple
avec des arbres binaires de recherche(Arbres rouges-noirs en particulier).
L'ajout(cf. union) se ferait alors en O(log n) et donc list_vars serait en O( k + n*log(n) )