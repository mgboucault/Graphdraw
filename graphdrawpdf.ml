(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphicspdf;;


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
	mutable vertex_color : color;
	(*épaisseur des flèches partant de ce noeud*)
	mutable edge_width : int;
	(*couleur des flèches partant de ce noeud*)
	mutable edge_color : color;
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
			
			
(**fonction dessinant les noeuds**)
let rec drawpdf_vertices vertices =
	set_line_width 1;
	match vertices with
		|[] -> ()
		|h_vertex::t_vertices ->
			(*si on a un empty_vertex, on ne trace rien et on passe à l'élément suivant*)
			if h_vertex.name = "empty_vertex" then 
				drawpdf_vertices t_vertices
			(*sinon, on trace lenoeud selon sa forme que l'on adapte au texte*)
			else
				let x = h_vertex.abscissa in
				let y = h_vertex.ordinate in
				let (w,h) = text_size h_vertex.name in
				set_color white;
				fill_rect (x - 2 * w/3) (y - h) (4 * w/3) (2 * h);
				set_color h_vertex.draw.vertex_color;
				match h_vertex.draw.vertex_shape with
					|Rectangle ->
						fill_rect (x - 2 * w/3) (y - h) (4 * w/3) (2 * h);
						set_color black;
						draw_rect (x - 2 * w/3) (y - h) (4 * w/3) (2 * h);
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					|Ellipse -> 
						fill_ellipse x y (2 * w/3) h;
						set_color black;
						draw_ellipse x y (2 * w/3) h;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					|Hexagon -> 
						let hexagon = [|(x-2*w/3,y);(x-w/2,y+h);(x+w/2,y+h);(x+2*w/3,y);(x+w/2,y-h);(x-w/2,y-h)|] in
						fill_poly hexagon;
						set_color black;
						draw_poly hexagon;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					| Parallelogram -> 
						let parallelogram = [|(x-w/2,y+h);(x+2*w/3,y+h);(x+w/2,y-h);(x-2*w/3,y-h)|] in
						fill_poly parallelogram;
						set_color black;
						draw_poly parallelogram;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					| Parallelogram_inv -> 
						let parallelogram_inv = [|(x-2*w/3,y+h);(x+w/2,y+h);(x+2*w/3,y-h);(x-w/2,y-h)|] in
						fill_poly parallelogram_inv;
						set_color black;
						draw_poly parallelogram_inv;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					|Trapezoid -> 
						let trapezoid = [|(x-w/2,y+h);(x+w/2,y+h);(x+2*w/3,y-h);(x-2*w/3,y-h)|] in
						fill_poly trapezoid;
						set_color black;
						draw_poly trapezoid;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices
					|Trapezoid_inv -> 
						let trapezoid_inv = [|(x-2*w/3,y+h);(x+2*w/3,y+h);(x+w/2,y-h);(x-w/2,y-h)|] in
						fill_poly trapezoid_inv;
						set_color black;
						draw_poly trapezoid_inv;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						drawpdf_vertices t_vertices;;
					
					
(**fonction dessinant les flèches**)				
let rec drawpdf_edges vertices =
	match vertices with
		|[] -> ()
		|h_vertex::t_vertices -> (
			set_line_width h_vertex.draw.edge_width;
			set_color h_vertex.draw.edge_color;
			let (x1, y1) = (h_vertex.abscissa, h_vertex.ordinate) in
			let (w1,h1) = text_size h_vertex.name in
			(*si on a un empty_vertex, on trace une droite à partir de (x1, y1)*)
			if h_vertex.name = "empty_vertex" then (
				if h_vertex.next_vertices = [] then 
					drawpdf_edges t_vertices
				else 
				let hn_vertex = List.hd h_vertex.next_vertices in
				let (x2, y2) = (hn_vertex.abscissa, hn_vertex.ordinate) in
				let (_,h2) = text_size hn_vertex.name in
				(*si le successeur est aussi un empty_vertex, on ne trace qu'une droite*)
				if hn_vertex.name = "empty_vertex" then
					draw_segments [|(x1, y1, x2, y2)|]
				(*sinon, on trace une flèche*)
				else
					(*selon la position relative des deux noeuds à tracer, on modifie les flèches*)
					(if y1 > y2 then draw_segments [|(x1, y1, x2, y2 + 2*h2);
													(x2, y2 + 2*h2, x2, y2 + h2);
													(x2, y2 + h2, x2 - 7, y2 + h2 + 7);
													(x2, y2 + h2, x2 + 7, y2 + h2 + 7)|]
					else draw_segments [|(x1, y1, x2, y2 - 2*h2);
										(x2, y2 - 2*h2, x2, y2 - h2);
										(x2, y2 - h2, x2 - 7, y2 - h2 - 7);
										(x2, y2 - h2, x2 + 7, y2 - h2 - 7)|] );
				(*on appelle à nouveau la fonction pour tracer les autres flèches*)
				drawpdf_edges t_vertices)
			(*sinon, on s'intéresse aux successeurs de l'élément actuel*)			
			else
				match h_vertex.next_vertices with
					(*s'il n'a pas de successeur, on ne trace rien et on passe à l'élément suivant*)
					|[] -> drawpdf_edges t_vertices
					(*s'il a un successeur, on regarde s'il s'agitt d'un empty_vertex ou non*)
					|hn_vertex::tn_vertices ->
						let (x2, y2) = (hn_vertex.abscissa, hn_vertex.ordinate) in
						let (_,h2) = text_size hn_vertex.name in
						(*s'il s'agit d'un empty_vertex, on trace une droite reliant le noeud actuel à son successeur*)
						if hn_vertex.name = "empty_vertex" then
							(*selon la position relative des deux noeuds à tracer, on modifie la droite*)
							if y1 > y2 then 
								draw_segments [|(x1, y1 - h1, x2, y2);|]
							else 
								draw_segments [|(x1, y1 + h1, x2, y2)|]
						(*s'il ne s'agit pas d'un empty_vertex, on trace une flèche reliant le noeud à son successeur*)
						else
							(*si le noeud est son propre successeur*)
							if h_vertex.name = hn_vertex.name then
								(draw_segments [|(x1, y1 - h1, x1, y1 - 2*h1);
												(x2, y2 + 2*h2, x2, y2 + h2);
												(x2, y2 + h2, x2 - 7, y2 + h2 + 7);
												(x2, y2 + h2, x2 + 7, y2 + h2 + 7)|];
								moveto x1 (y1-2*h1);
								curveto (x1+2*w1,y1-2*h1) (x1+2*w1,y1+2*h1) (x2, y2+2*h2) )
							(*si le noeud et le successeur sont différents*)	
							else
								(*selon la position relative des deux noeuds à tracer, on modifie les flèches*)
								if y1 > y2 then draw_segments [|(x1, y1 - h1, x2, y2 + 2*h2);
																(x2, y2 + 2*h2, x2, y2 + h2);
																(x2, y2 + h2, x2 - 7, y2 + h2 + 7);
																(x2, y2 + h2, x2 + 7, y2 + h2 + 7)|]
									else draw_segments [|(x1, y1 + h1, x2, y2 - 2*h2);
														(x2, y2 - 2*h2, x2, y2 - h2);
														(x2, y2 - h2, x2 - 7, y2 - h2 - 7);
														(x2, y2 - h2, x2 + 7, y2 - h2 - 7)|];
						(*on crée un nouveau noeud contenant le noeud actuel auquel on a retiré son premier successeur*)
						let new_vertex = {name = h_vertex.name; next_vertices = tn_vertices; abscissa = h_vertex.abscissa; ordinate = h_vertex.ordinate; draw = h_vertex.draw} in
							(*on appelle à nouveau la fonction pour tracer les autres flèches*)
							drawpdf_edges (new_vertex::t_vertices) );;

				
(**fonction dessinant et reliant les ports**)
let rec drawpdf_ports ports vertices=
	match ports with
		|[] -> ()
		|h_port::t_ports ->
			(*pour se faciliter le dessin par la suite, on ne prend que les out_port*)
			if h_port.io = "in" then drawpdf_ports (t_ports@[h_port]) vertices
			else	
				(*on cherche l'in_port correspondant au port actuel*)
				(let port = List.find (fun p -> p.port_name = h_port.port_name) t_ports in
				(*on cherche le noeud correspondant à l'out_port*)
				let h_vertex = List.find (fun v -> v.name = h_port.vertex_name) vertices in
				set_line_width h_port.width;
				set_color h_port.color;
				(*on cherche le noeud correspondant à l'in_port*)
				let vertex = List.find (fun v -> v.name = port.vertex_name) vertices in
				let (x1, y1) = (h_vertex.abscissa, h_vertex.ordinate) in
				let (x2, y2) = (vertex.abscissa, vertex.ordinate) in
				let (w1, h1) = text_size h_vertex.name in
				let (w2, h2) = text_size vertex.name in
				(*on trace les ports selon leur position relative*)
				match x1 < x2 with
					|true ->
						draw_poly [|(x1+2*w1/3, y1+5);(x1+2*w1/3, y1-5);(x1+2*w1/3+8, y1)|];
						draw_poly [|(x2-2*w2/3, y2);(x2-2*w2/3-8, y2+5);(x2-2*w2/3-8, y2-5)|];
						draw_segments[|(x1+2*w1/3+8, y1, x2-2*w2/3-8, y2)|];
						set_color black;
						if y1 > y2 then
							(let (w, h) = text_size ("in_"^port.port_name) in
							moveto (x1+2*w1/3+8) y1;
							draw_string ("out_"^h_port.port_name);
							moveto (x2-2*w2/3-8-w) (y2-h);
							draw_string ("in_"^port.port_name)) 
						else
							(let (w, _) = text_size ("in_"^port.port_name) in
							let (_, h) = text_size ("out_"^h_port.port_name) in
							moveto (x1+2*w1/3+8) (y1-h);
							draw_string ("out_"^h_port.port_name);
							moveto (x2-2*w2/3-8-w) y2;
							draw_string ("in_"^port.port_name) );
						(*on trace les ports qui n'ont pas encore été tracés*)
						let otherports = List.filter (fun p -> p <> port) t_ports in
						drawpdf_ports otherports vertices
					|false ->
						draw_poly [|(x1-2*w1/3, y1+5);(x1-2*w1/3, y1-5);(x1-2*w1/3-8, y1)|];
						draw_poly [|(x2+2*w2/3, y2);(x2+2*w2/3+8, y2+5);(x2+2*w2/3+8, y2-5)|];
						draw_segments[|(x1-2*w1/3-8, y1, x2+2*w2/3+8, y2)|];
						set_color black;
						if y1 < y2 then
							(let (w, h) = text_size ("out_"^h_port.port_name) in
							moveto (x1-2*w1/3-8-w) (y1-h);
							draw_string ("out_"^h_port.port_name);
							moveto (x2+2*w2/3+8) y2;
							draw_string ("in_"^port.port_name)) 
						else
							(let (w, _) = text_size ("out_"^h_port.port_name) in
							let (_, h) = text_size ("in_"^port.port_name) in
							moveto (x1-2*w1/3-8-w) y1;
							draw_string ("out_"^h_port.port_name);
							moveto (x2+2*w2/3+8) (y2-h);
							draw_string ("in_"^port.port_name) );
						(*on trace les ports qui n'ont pas encore été tracés*)
						let otherports = List.filter (fun p -> p <> port) t_ports in
						drawpdf_ports otherports vertices );;
						
						
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
	drawpdf_vertices vertices;
	(*on trace les flèches*)
	drawpdf_edges vertices;
	(*on trace et on relie les ports*)
	drawpdf_ports ports vertices;
	(*on rétablit les anciennes coordonnées des noeuds*)
	rescalepdf vertices;
	(*on ferme la fenêtre, ce qui enregistre le graphe en format pdf*)
	close_graph ();;
