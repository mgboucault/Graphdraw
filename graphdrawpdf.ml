(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphicspdf;;
open Types;;


(**Définition des fonctions draw_vertices, draw_edges et draw_ports**)
module GraphDrawPDF = Makegraphics.Make (Graphicspdf);;


(**fonction modifiant les coordonnées des noeuds**)
let scalepdf vertices =
	(*on modifie les coordonnées des noeuds pour qu'ils occupent tout l'espace*)
	let width = size_x() in
	let f vertex =
		vertex.abscissa <- vertex.abscissa - 7*width/36;
		vertex.ordinate <- vertex.ordinate;
	in
	List.iter f vertices;;
			

(**fonction rétablissant les coordonnées des noeuds**)
let rescalepdf vertices =
	(*on remodifie les coordonnées des noeuds pour qu'ils soient afficher correctement dans la fenêtre*)
	let width = size_x() in
	let f vertex =
		vertex.abscissa <- vertex.abscissa + 7*width/36;
		vertex.ordinate <- vertex.ordinate;
	in
	List.iter f vertices;;
							
						
(**fonction enregistrant le graphe en format pdf**)
let save_pdf title width height ports vertices =
	(*initialisation de la taille et du titre du pdf*)
	open_pdf (title^".pdf");
	open_graph (" "^string_of_int (29*width/36)^"x"^string_of_int height);
	(*on efface tout ce qui pourrait se trouver sur le pdf*)
	set_color white;
	fill_rect 0 0 (29*width/36) height;
	(*on adapte les coordonnées des noeuds*)
	scalepdf vertices;
	(*on trace les noeuds*)
	GraphDrawPDF.draw_vertices vertices;
	(*on trace les flèches*)
	GraphDrawPDF.draw_edges vertices;
	(*on trace et on relie les ports*)
	GraphDrawPDF.draw_ports ports vertices;
	(*on rétablit les anciennes coordonnées des noeuds*)
	rescalepdf vertices;
	(*on ferme la fenêtre, ce qui enregistre le graphe en format pdf*)
	close_graph ();;
