(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphicspdf


type port = {
	(*nom du port*)
	port_name : string;
	(*noeud auquel le port est attaché*)
	vertex_name : string;
	(*définit s'il s'agit d'un port qui reçoit ou qui envoie des données ("in"/"out")*)
	io : string;
	(*épaisseur du trait*)
	mutable width : int;
	(*couleur du port*)
	mutable color : color
}


type shape = Rectangle 
			| Ellipse
			| Hexagon
			| Parallelogram
			| Parallelogram_inv
			| Trapezoid
			| Trapezoid_inv
			

type drawing_option = {
	(*forme du noeud*)
	mutable vertex_shape : shape;
	(*couleur du noeud*)
	mutable vertex_color : color;
	(*épaisseur des flèches partant de ce noeud*)
	mutable edge_width : int;
	(*couleur des flèches partant de ce noeud*)
	mutable edge_color : color
}


type vertex = {
	(*nom du noeud*)
	name : string;
	(*liste de ses successeurs*)
	mutable next_vertices : vertex list;
	(*abscisse du noeud*)
	mutable abscissa : int;
	(*ordonnée du noeud*)
	mutable ordinate : int;
	(*option de dessin du noeud*)
	draw : drawing_option
}


(**fonction modifiant les coordonnées des noeuds**)
val scalepdf : vertex list -> unit


(**fonction rétablissant les coordonnées des noeuds**)
val rescalepdf : vertex list -> unit


(**fonction dessinant les noeuds**)
val drawpdf_vertices : vertex list -> unit
					
					
(**fonction dessinant les flèches**)
val drawpdf_edges : vertex list -> unit


(**fonction dessinant et reliant les ports**)
val drawpdf_ports : port list -> vertex list -> unit
		
							
(**fonction enregistrant le graphe en format pdf**)
val save_pdf : string -> int -> int -> port list -> vertex list -> unit
