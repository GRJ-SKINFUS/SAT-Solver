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

Q31 - La formule générée est sous FNC si les formules intiales sont des variables.
Si il y a n formules au départ, la taille de la formule générée est en O(n²).
