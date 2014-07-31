(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphicspdf
open Types


(**fonction modifiant les coordonnées des noeuds**)
val scalepdf : vertex list -> unit


(**fonction rétablissant les coordonnées des noeuds**)
val rescalepdf : vertex list -> unit
		
							
(**fonction enregistrant le graphe en format pdf**)
val save_pdf : string -> int -> int -> port list -> vertex list -> unit
