type formule =
	| Var of string
	| Top
	| Bot
	| And of formule * formule
	| Or of formule * formule
	| Not of formule

let implique (f1, f2) = Or(Not f1, f2)
let equivalence (f1, f2) = And(implique (f1, f2), implique (f2, f1))

exception Erreur_syntaxe
exception Fichier_invalide
exception Erreur_valuation

(* Symboles:
	'T' -> true
	'F' -> false
	'&' -> And
	'|' -> Or
	'~' -> Not
	'>' -> implication
	'=' -> equivalence
 *)

type valuation = (string*bool) list

(* Détermine si c correspond à un opérateur binaire logique *)
let is_binop (c: char) : bool = match c with 
	| '&' |  '|' |  '>' |  '='  -> true
	| _ -> false 

(* Priorité de l'opérateur c. Permet de déterminer
	comment interpréter une formule sans parenthèses.
	Par exemple, "x&y|z" sera interprété comme "(x&y)|z"
	car & est plus prioritaire que | *)
let priority (c: char) : int = match c with
	| '&' -> 4
	| '|' -> 3
	| '=' -> 2
	| '>' -> 1
	| _ -> raise Erreur_syntaxe (* c n'est pas un opérateur *)

(* indice de l'opérateur le moins prioritaire parmis ceux
   qui ne sont pas entre parenthèses entre s.[i] et s.[j] 
   inclus *)
 let find_op_surface (s: string) (i: int) (j: int) : int =
 	(* 
 	   Renvoie l'indice de l'opérateur le moins prioritaire entre
 	   i et j, sachant que res est l'indice du meilleur opérateur
 	   entre i et k-1.
 	   paren_lvl: niveau d'imbrication actuel des parenthèses *)
 	let rec find_op_paren (k:int) (res:int) (paren_lvl: int) : int  =
 		if k=j+1 then res else
 		if s.[k] = '(' then find_op_paren (k+1) res (paren_lvl+1)
 		else if s.[k] = ')' then find_op_paren (k+1) res (paren_lvl-1) 

 		(* Le caractère lu est pris si l'on est hors des parenthèses,
 		   que le caractère est bien un opérateur, et qu'il est moins
 		   prioritaire que le meilleur résultat jusqu'ici *)
 		else if paren_lvl = 0 
 			 && is_binop s.[k] 
 			 && (res = -1 || priority s.[k] < priority s.[res]) 
 			 then find_op_paren (k+1) k (paren_lvl)
 		else find_op_paren (k+1) res (paren_lvl)
 	in find_op_paren i (-1) 0;;

(* Renvoie une formule construite à partir de la chaîne s.
   Lève une exception Erreur_syntaxe si la chaîne ne représente pas une formule valide. *)
let parse (s: string) : formule =
	let n = String.length s in
	(* construit une formule à partir de s[i..j] *)
	let rec parse_aux (i: int) (j:int) =
		if not (0 <= i && i < n && 0 <= j && j < n && i <= j ) then raise Erreur_syntaxe else
		if s.[i] = ' ' then parse_aux (i+1) j
		else if s.[j] = ' ' then parse_aux i (j-1)
		else let k = find_op_surface s i j in 
		if k = -1 then
			if s.[i] = '~' then 
				Not (parse_aux (i+1) j)
			else if s.[i] = '(' then
				begin 
					if (s.[j] != ')') then (print_int j; failwith "mauvais parenthésage") else
					parse_aux (i+1) (j-1)
				end
			else if (i = j && s.[i] = 'T') then Top
			else if (i = j && s.[i] = 'F') then Bot
			else let nom_variable = String.sub s i (j-i+1) in 
			if String.contains nom_variable ' ' then raise Erreur_syntaxe else Var nom_variable

		else match s.[k] with
			| '&' -> And(parse_aux i (k-1), parse_aux (k+1) j)
			| '|' -> Or(parse_aux i (k-1), parse_aux (k+1) j)
			| '=' -> equivalence(parse_aux i (k-1), parse_aux (k+1) j)
			| '>' -> implique(parse_aux i (k-1), parse_aux (k+1) j)
			| _ -> raise Erreur_syntaxe
	in parse_aux 0 (String.length s -1)

(* Renvoie une formule construire à partir du contenu du fichier fn.
   Lève une exception Erreur_syntaxe si le contenu du fichier n'est pas une formule valide.
   Lève une exception Sys_error(message_erreur) si le nom du fichier n'est pas valide. *)
let from_file (filename: string) : formule = 
	(* concatène toutes les lignes de f en une seule chaîne *)
	let rec read_lines f = 
		try 
			let next_line = input_line f in
			let s = read_lines f in
			next_line ^ s
		with 
			| End_of_file -> ""
	in
	let f = open_in filename in 
	let s = read_lines f in
	parse s

let test_parse () =
	assert (parse "a | (b & ~c)" = Or(Var "a", And(Var "b", Not (Var "c"))));
	assert (parse "F |     T  " = Or(Bot, Top));
	assert (parse "a = (b > c)" = equivalence(Var "a", implique(Var "b", Var "c")));

	try 
		let _ = parse "a b > c" in ();
	with Erreur_syntaxe -> ();
	try 
		let _ = parse "((a | F) & c" in ();
	with Failure msg -> print_string "Erreur de parenthésage ? : "; print_endline msg; ();

	print_string "Tests parse OK\n";;

let test_from_file () =
	assert(from_file "tests/test1.txt" = implique (And (Or (Var "a", Var "b"), Not (Var "c")  ) , Or (Var "d", Var "b")) );
	assert(from_file "tests/test2.txt" = Or (Var "a", Or (Var "b" , Var "c")) );

	print_string "Tests from_file OK\n";;


(* Renvoie le contenu du fichier fn sous forme de string.
Le fichier ne doit contenir qu'une seule liigne.*)
let read_file (fn: string) : string =
    let ic = open_in fn in 
    let res = input_line ic in 
    close_in ic; res

(* Renvoie le nombre de ET OU et NON dans une formule *)
let compt_ops(f: formule) : int =
	(*Retourne n + compte_ops f *)
	let rec compte_rec (f: formule)(n: int) : int =
		match f with
		| Var _ | Top| Bot -> n
		| And (fg, fd) | Or (fg, fd) ->
			let ng = compte_rec fg n in compte_rec fd ng
		| Not f' -> compte_rec f' (n+1)

	in compte_rec f 0


(* Vérifie si une liste est strictement croissante (part. pas de doublons)*)
let verify_list(l: 'a list) : bool =
	(* Vérifie si une liste est strictement croissante, en comprenant au debut de la liste y
	si x = Some(y), si x = None il est ignoré *)
	let rec verify_preced(l: 'a list)(x: 'a option) : bool =
		match l, x with 
		| [], _ -> true
		| a::l', Some(y) ->
			if y < a then verify_preced l' (Some a)
			else false
		| a::l', None -> verify_preced l' (Some a)
	in verify_preced l None
	
(* Si l1 et l2 sont deux listes strictement croissantes, renvoie la liste 
   strictement croissante issue de l'union des éléments de l1 et l2 *)
let union(l1: 'a list)(l2: 'a list) : 'a list =
	(*Renvoie la concatenation de lres et union(l1, l2) *)
	let rec union_concat(l1: 'a list)(l2: 'a list)(lres: 'a list) : 'a list =
		match l1, l2 with
		| [], _ -> lres @ l2
		| _, [] -> lres @ l1
		| a::l1' , b::l2' ->
			if a < b then 	   union_concat l1' l2 (a::lres)
			else if b > a then union_concat l1 l2' (b::lres)
			else			   union_concat l1' l2' (a::lres)		
	in union_concat l1 l2 [] 	

(* Renvoie la liste des variables sans doublons d'une formule *)
let list_vars(f: formule) : string list =
	(* Met à jour la liste des variables de l avec celles contenues dans f *)
	let rec list_vars_update(f: formule)(l: string list) : string list =
		match f with
		| Var var -> union [var] l
		| Top | Bot -> l
		| Not f' -> list_vars_update f' l
		| And (fg, fd) | Or (fg, fd) ->
			let l' = list_vars_update fg l in list_vars_update fd l'
	in list_vars_update f []

(* Interprete une formule dans une valuation, si la valuation n'est pas valide raise Not_found *)
let rec interpreter(f: formule)(v: valuation) : bool =
	match f with 
	| Var var -> List.assoc var v
	| Top -> true
	| Bot -> false
	| Not f' ->   not (interpreter f' v)
	| And (fg, fd) -> (interpreter fg v) && (interpreter fd v)
	| Or (fg, fd) ->  (interpreter fg v) || (interpreter fd v)

(* Si bl représente l'écriture binaire de x, add_one(bl) représente celle de x + 1 
   Attention : le premier element de bl est le bit de poids faible *)
let rec add_one(bl: bool list) : bool list =
	match bl with
	| [] -> [true]
	| b::bl' -> 
		if b then 
			false::(add_one bl')
		else true::bl'

(* Renvoie une liste de bool associée à la valuation v *)
let valuation_to_bool(v: valuation) : bool list =
		List.map snd v

let valuation_next(v: valuation) : valuation option =
		let b = add_one(valuation_to_bool v) in
		(* Remplis la valuation v avec les valeurs de l, si le nombre de variables et de bools
			 ne coincident pas raise valuation maximale *)
		let rec remplir_val(v: valuation)(l: bool list) : valuation =
				match v,l with
					| [],[] -> []
					| (name,_)::v', b::l' ->
						(name,b)::(remplir_val v' l')
					| _ -> raise Erreur_valuation

		in 
			try Some(remplir_val v b)
			with Erreur_valuation -> None

(* Renvoie la valuation ou toute les variables sont mises à fausses *)
let valuation_init(vars: string list) : valuation =
		let f var = (var, false) in
		List.map f vars

let test () =
    test_parse ();
	test_from_file ();
    print_string "Tous les tests ont réussi\n"

let main ()=
    if Array.length Sys.argv = 0 then failwith "Execution sans arguments" else begin
    if Sys.argv.(0) = "test" then test() 
    else print_string (read_file Sys.argv.(0))
    end

let _ = main () (* exécution de la fonction main *)