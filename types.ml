(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


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
	mutable color : Graphics.color
};;


type shape = Rectangle 
			| Ellipse
			| Hexagon
			| Parallelogram
			| Parallelogram_inv
			| Trapezoid
			| Trapezoid_inv;;
			

type drawing_option = {
	(*forme du noeud*)
	mutable vertex_shape : shape;
	(*couleur du noeud*)
	mutable vertex_color : Graphics.color;
	(*épaisseur des flèches partant de ce noeud*)
	mutable edge_width : int;
	(*couleur des flèches partant de ce noeud*)
	mutable edge_color : Graphics.color;
};;


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
};;
