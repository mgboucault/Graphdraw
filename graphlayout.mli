open Graphdrawpdf


(** Sugiyama algorithm **)


(** I  : Cycle removal **)

(** I-a: Préliminaire **)

(* permet d'enlever les occurences multiples de la liste l*) 
val no_occ : 'a list -> 'a list

(*Permet de calculer le max d'une liste d'entier*)
val list_max : int list -> int 

(* permet d'enlever les occurences multiples pour une liste non ordonnée*)
val no_occ_eq : 'a list list -> 'a list list

(* Création de la liste des noeuds à partir du graph *)
val list_noeud : ('a * 'a) list -> 'a list

(** I-b: Modélisation **)

(* conversion d'un graph en fonction: entrée liste d'éléments
suivant graph ["a";"b"] = ["b";"d";"e";"f"] *)
val suivant : ('a * 'b) list -> 'a list -> 'b list

(* Création d'une liste de liste à partir de la première liste en ajoutant un élément de la deuxième
ex:
completer [1;2;3] [4;5] = [ [4;1;2;3] ; [5;1;2;3] ]  
*)
val completer : 'a list -> 'a list -> 'a list list

(* affirer tout les chemains / cycle qui parte d'un noeud (origine) *)
val chemin : ('a * 'a) list -> 'a -> 'a list list

(* Chercher tous les cycle du graph déduit de "liste_lien"*)
val list_cycle : ('a * 'a) list -> 'a list list

(** I-c: Greedy algorithm ( trouver les flèches + choix/modification des flèches) **)

(*Permet de trouver les flèches qui forment les cycles*)
val trouver_fleches : 'a list list -> ('a * 'a) list

(*Permet de donner pour chaque flèches le nombre de cycles dans lesquels elle est comprise
Cet fonction est l'heuristique à utiliser pour le greedy algotithme*)
val occurrence_fleches : ('a * 'a) list -> ('a * 'a * int) list


(*
Permet de retourner des flèches pour rendre le graphe acyclique
heuristique: on inverse les flèche qui influencent le plus de cycles

	entree: liste_lien = (sting*string) list -> liste des flèches du graphe
	
	sortie: ( (sting*string) list )*( (sting*string) list  )   =   ( liste_lien_sortie , liste_fleches )
					liste_lien_sortie = liste des lien du graphe acyclique
					liste_fleches     = liste des flèches modifées pour rendre le graphe acyclique
*)
val cycle_removal : ('a * 'a) list -> ('a * 'a) list * ('a * 'a) list

(** II  : Layering **)

(** II-a Préliminaire**)


(*Conversion du tableu sous forme regroupé
exemple :
[(a,b);(a,c);(b,d)]->[ (a,[b;c]) ; (b,[d]) ; (c,[]) ; (d,[]) ]
*)
val table : ('a * 'a) list -> ('a * 'a list) list

(*Conversion graph tab -> liste *)
val list_of_graph : ('a * 'a list) list -> ('a * 'a) list

(**II-b Manipulation de graphe**)

(*Permet de modifier la coordinate d'un vertex dans le graph comme liste des positions (liste_position)  
modifier [(a,0,1);(b,2,3)] 1 a 4 = [(a,4,1);(b,2,3)]
modifier [(a,0,1);(b,2,3)] 2 b 4 = [(a,0,1);(b,2,4)]
*)
val modifier : ('a * 'b * 'b) list -> int -> 'a -> 'b -> ('a * 'b * 'b) list

(*Permet de déposer tous les bouts d'un graphe (sous forme de liste de lien ) sur un étage du graphe (liste_position) *)
val poser : 'a -> ('b * 'a * 'a) list -> ('b * 'c list) list -> ('b * 'a * 'a) list * 'b list

val enlever_tab : ('a * 'a list) list -> 'a list -> ('a * 'a list) list

(** II-c longest path layering **)

(* Définir l'ordonnée de chaque vertex, selon la methode de "longest path layering" 
entree: liste_lien    (sintg1,sting2) list
sortie: liste position   (sting,int1,int2) list 
*)

val layering : ('a * 'a) list -> ('a * int * int) list

(*Placement simple des noeuds horizontalements -> chaque noeud a un emplacement distinct*)
val placement_h0 : ('a * 'b * int) list -> ('a * int * int) list

(** III- Regroupement des informations **)

(**II-a Traductuion vertex **)


(*Converti un tableau de lien en liste de vertex : pour graphe non cyclique *)
val vertex_of_graph' : (string * string) list -> vertex list

(** III- Réduction des chemins    **)

(*Permet de placer le noeud vertex en (x,y) dans le graphe graph_vertex*)
val move : vertex -> int -> int -> vertex list ref -> unit

(*Permet de recuperer la largeur de chaque étage d'un vertex*)
val info : vertex list -> int list

(*Permet de faire +1 sur le n eme element d'une liste d'entier pointé*)
val upp : int list ref -> int -> unit
			
(*fonction  qui ordonne les graphes des vertices*)
val ordonner : vertex list ref -> unit

val reduce_path : vertex list ref -> unit 

val change_layer00 : (string * int * int) list -> vertex list ref -> unit

(** III-b Ajouter les noeuds virtuels dans un graphe vertex **)

(*Ajoute les noeuds virtuels dans la liste de vertex*)
val add_dummy : vertex list ref -> unit list


(** III-d Crossing reduce **)

(*Permet d'obtenir les lien entre deux étages*)
val layer  : vertex list -> int -> ((string * int * int) * (string * int * int) list) list
	
(*fonction qui donne une liste de liens d'un étage : père des fils*)
val layer' : vertex list -> int -> ((string * int * int) * (string * int * int) list) list

(*fonction qui reduit les croisement grossièrement : logique fils->pere
val less_crossing  : vertex list ref -> unit*)

(*fonction qui reduit les croisement grossièrement : logique pere->fils*)
val less_crossing : vertex list ref -> unit

(*fonction qui reduit les croisement grossièrement : logique pere->fils*)
val less_crossing' : vertex list ref -> unit

(**IV Finalisation**)

(**IV- a Centrer les graphes **)

val center_fixed_father    : vertex list ref ->int-> unit
val center_fixed_son   : vertex list ref ->int-> unit

val center              : vertex list ref      -> unit

  
(**IV- ? Re-inserer les cycles **)

(*fonction qui permet de remettre les flècges dans le bon sens*)
val change_arrow : (string * string) list -> vertex list ref -> unit
 

(*Converti un tableau de lien en liste de vertex : pour graphe non cyclique *)
val vertex_of_graph : (string * string) list -> vertex list

val remove_empty_vertex : vertex list ref -> unit
(**IV- b: Fonction de trie / creation finale **)

val add_link_list : (string * string) list -> vertex list -> vertex list
(*let verifier graph = List.map (fun v -> (v.name , v.abscissa , v.ordinate)) graph;;*)

val graph_layout : (string * string) list -> vertex list
