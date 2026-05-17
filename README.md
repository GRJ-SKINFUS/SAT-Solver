# SAT-Solver

DM3 - Conception et utilisation d'un SAT-Solver

----- COMPILATION

Les 3 commandes pour compiler chaque partie du projet sont les suivantes :

1. SAT-Solver
   ocamlc satsolver.ml -o satsolver

2. Problème des n dames
   gcc n_dames.c utils.c

3. Problème des maisons
   gcc cinqmaisons.c utils.c

----- UTILISATION

1. SAT-Solver

   L'exécution du solver prend en argument 1 ou 2 paramètres:
   1. "test" -> Permet de lancer toutes les batteries de tests internes au code ocaml du satsolver qui testent toutes les fonctions intermédiaires.
   2. "basic/opt/fnc" <nom_fichier> -> Lance le solver sur la formule contenu dans le fichier spécifié et tente une résolution avec le mode spécifié :
      - basic -> algorithme de quine naïf
      - opt -> algorithme de quine avec choix intelligents de variables et de valeurs
      - fnc -> optimisation spécialisée en formules sous FNC

2. n_dames

   L'exécution de la génération du problème des n dames prend en argument n (taille du plateau) puis le nom du fichier à générer.

3. cinq maisons

   L'exécution du problème des cinq maisons prend uniquement le nom de fichier à générer en argument.

----- STRUCTURATION

Le projet est divisé en 3 grandes parties :

- Le sat-solver, dans le dossier /satsolver dans un unique fichier ocaml qui contient l'ensemble du code relatif à la résolution du problème SAT sur des formules logiques.

- Le problème des dames, dans le dossier /problemes qui permet de générer des formules sur le problèmes des dames.

- Le problème des cinq maisons, aussi dans /problemes permettant de générer des formules pour le problème des cinq maisons.

Ces deux dernières parties du projet partagent le code de utils.c regroupant des fonctions utiles à la génération de formules logiques :
au moins, au plus, et, ou sur des listes de variables et écriture de fichiers.
