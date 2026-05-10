RAPPORT

Q10 -
Pour une formule avec n variables et k opérateurs,
la fonction effectue des opérations en O(1) pour chaque opérateurs.
Et pour chaque variable on réalise "union" avec le reste des variables.
Or union s'effectue en 0(n) pour des listes de taille n.
Donc list_vars s'effectue en O(k + n\*\*2).

Pour améliorer la complexité on pourrait utiliser des dictionnaires, implémentés par exemple
avec des arbres binaires de recherche(Arbres rouges-noirs en particulier).
L'ajout(cf. union) se ferait alors en O(log n) et donc list_vars serait en O( k + n\*log(n) )

Q19 -
En théorie on ne visite que chaque noeuds de l'arbre de la formule une seule fois, cependant à chaque appel de simpl_step, on repart du haut de l'arbre. Les familles en 0(n²) sont donc des familles où à chaque étape, la simplification se fait tout en bas de l'arbre ce qui oblige l'algorithme à tout redescendre à chaque fois par récursivité. Par exemple, des familles du type:

- And (And (And (..., Top), Top), Top)
- Or (Or (Or (..., Bot), Bot), Bot)

Q20 -
On résoud ce problème de complexité en descendant une unique fois l'arbre: on fusionne les étapes de simplification et de récursion.

Q25 -
Maintenant, au lieu de choisir les variables dans l'odre alphabétique et de tester d'abort T puis F, essayons d'implémenter une stratégie plus optimisée:

Pour le choix de la variable, prenons celle qui apparait le plus de fois dans la formule afin d'espérer obtenir les plus grandes simplifications dès les premières étapes.
Pour choisir si on teste d'abord T ou F, on génère les 2 formules et on teste celle qui est la plus courte (qui contient le moins d'opérateurs).

Théoriquement, cela permet de réduire la complexité asymptotique. En pratique, cela rajoute pas mal d'opérations à chaque tour et n'est pas toujours le plus efficace. En pratique, on choisira l'algorithme basique pour éviter les grandes constantes de complexité.

Q31 -
La formule générée est sous FNC si les formules intiales sont des variables.
Si il y a n formules au départ, on aura 2 parmi n conjonctions la taille de la formule générée est en O(n²).

Q38 -
Donnons la taille de la formule générée pour le problème des n dames, en fonction de n.

Pour n expressions atomiques, la fonction au_moins_une renvoie une formule de taille n.
En effet on réalise simplement la disjonction de chaque expression.

Pour n expressions atomiques, la fonction au_plus_une renvoie une formule de taille n². (question précédente)

Ainsi, la taille de la contrainte sur une ligne est en O(n + n²) = O(n²) car exactement une reine = au moins une et au plus une
Sur chaque colonne, la formule est en O(n²) car on regarde au plus une reine.
Sur chaque diagonale, c'est comme sur les colonnes, O(n²).

De plus, on inscrit une contrainte sur chaque ligne, chaque colonne et chaque diagonale. Il y a n lignes, n colonnes et 2n-1 diagonales.

Finalement, la formule finale (en nombre d'expressions atomiques) est donc en O(n\*n² + n\*n + (2n-1)\*n²) = O(n^3)

Q40-
On vérifie effectivement que la formule du problème à 3 dames est insatisfiable, et on trouve des solutions pour le problème à 5 dames et à 8 dames:

Avec l'implémentation des choix de variable / true et false :

- pour 5 dames, en regardant ligne par ligne : 2, 5, 3, 1, 3 (trouvé en 1/4 de seconde)
- pour 8 dames : 6, 8, 2, 4, 1, 7, 5, 3 (trouvé en 20min)

Sans cette optimisations :

- pour 5 dames : 2, 4, 1, 3, 5 (trouvé en 1/100 secondes)
- pour 8 dames : 4, 7, 5, 2, 6, 1, 3, 8 (trouvé en moins d'1min)

On voit bien qu'en pratique l'algorithme simple est bien plus efficace.
