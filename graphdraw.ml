(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphics;;
open Graphdrawpdf;;
open Graphlayout;;


exception End;;


(**fonction ouvrant la fenêtre**)
let open_window title width height =
	(*ouverture de la fenêtre*)
	set_window_title title;
	open_graph (" "^string_of_int width^"x"^string_of_int height);;


(**fonction initialisant la fenêtre**)
let init () =
	(*création de deux espaces séparés*)
	let h_case = size_y() / 30 in
	let w_case = size_x() / 36 in
	set_line_width 3;
	set_color black;
	draw_rect 0 0 (7*w_case) (size_y()-3);
	draw_rect (7*w_case) 0 (size_x()-7*w_case-3) (size_y()-3);
	(*options de dessin*)
	set_line_width 1;
	moveto w_case (28*h_case);
	draw_string "Choose drawing options for :";
		(*bouton "Vertex"*)
		set_line_width 2;
		let (w,h) = text_size "Vertex" in
		moveto (3*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (3*w_case/2-2*w/3) (53*h_case/2-h) (4*w/3) (2*h);
		draw_string "Vertex";
		(*bouton "Edge"*)
		let (w,h) = text_size "Edge" in
		moveto (7*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (7*w_case/2-w) (53*h_case/2-h) (2*w) (2*h);
		draw_string "Edge";
		(*bouton "Port"*)
		let (w,h) = text_size "Port" in
		moveto (11*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (11*w_case/2-w) (53*h_case/2-h) (2*w) (2*h);
		draw_string "Port";
	(*bouton "Add vertex"*)
	let (w,h) = text_size "Add vertex" in
	moveto (7*w_case/2-w/2) (13*h_case-h/2);
	draw_rect (7*w_case/2-2*w/3) (13*h_case-h) (4*w/3) (2*h);
	draw_string "Add vertex";
	(*bouton "Remove vertex"*)
	let (w,h) = text_size "Remove vertex" in
	moveto (7*w_case/2-w/2) (11*h_case-h/2);
	draw_rect (7*w_case/2-2*w/3) (11*h_case-h) (4*w/3) (2*h);
	draw_string "Remove vertex";
	(*bouton "Link vertex"*)
	let (w,h) = text_size "Link vertex" in
	moveto (7*w_case/2-w/2) (9*h_case-h/2);
	draw_rect (7*w_case/2-2*w/3) (9*h_case-h) (4*w/3) (2*h);
	draw_string "Link vertex";
	(*bouton "Add port"*)
	let (w,h) = text_size "Add port" in
	moveto (7*w_case/2-w/2) (7*h_case-h/2);
	draw_rect (7*w_case/2-2*w/3) (7*h_case-h) (4*w/3) (2*h);
	draw_string "Add port";
	(*bouton "Help"*)
	set_color magenta;
	let (w,h) = text_size "Help" in
	moveto (5*w_case/2-w/2) (9*h_case/2-h/2);
	draw_rect (5*w_case/2-w) (9*h_case/2-h) (2*w) (2*h);
	draw_string "Help";
	(*bouton "Layout"*)
	set_color blue;
	let (w,h) = text_size "Layout" in
	moveto (9*w_case/2-w/2) (9*h_case/2-h/2);
	draw_rect (9*w_case/2-2*w/3) (9*h_case/2-h) (4*w/3) (2*h);
	draw_string "Layout";
	(*bouton "Save"*)
	set_color green;
	let (w,h) = text_size "Save" in
	moveto (5*w_case/2-w/2) (5*h_case/2-h/2);
	draw_rect (5*w_case/2-w) (5*h_case/2-h) (2*w) (2*h);
	draw_string "Save";
	(*bouton "Quit"*)
	set_color red;
	let (w,h) = text_size "Quit" in
	moveto (9*w_case/2-w/2) (5*h_case/2-h/2);
	draw_rect (9*w_case/2-w) (5*h_case/2-h) (2*w) (2*h);
	draw_string "Quit";
	(*bouton "Reset"*)
	set_color black;
	let (w,h) = text_size "Reset" in
	moveto (w/2) (h/2);
	draw_rect 0 0 (2*w) (2*h);
	draw_string "Reset";;


(**fonction initialisant la fenêtre lors du choix d'options pour les noeuds**)
let init_vertex () =
	let h_case = size_y() / 30 in
	let w_case = size_x() / 36 in
	set_color black;
	(*bouton "Vertex"*)
		set_line_width 2;
		let (w,h) = text_size "Vertex" in
		moveto (3*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (3*w_case/2-2*w/3) (53*h_case/2-h) (4*w/3) (2*h);
		draw_string "Vertex";
	(*choix de forme*)
		moveto (3*w_case) (24*h_case);
		draw_string "Shape :";
			draw_rect (3*w_case) (23*h_case) w_case (h_case-5);
			draw_ellipse (7*w_case/2) (45*h_case/2) (w_case/2) ((h_case-5)/2);
			let hexagon=[|(3*w_case,43*h_case/2-3);(10*w_case/3,22*h_case-5);(11*w_case/3,22*h_case-5);(4*w_case,43*h_case/2-3);(11*w_case/3,21*h_case);(10*w_case/3,21*h_case)|] in
			draw_poly hexagon;		
			let parallelogram = [|(10*w_case/3,21*h_case-5);(4*w_case,21*h_case-5);(11*w_case/3,20*h_case);(3*w_case,20*h_case)|] in
			draw_poly parallelogram;	
			let parallelogram_inv = [|(3*w_case,20*h_case-5);(11*w_case/3,20*h_case-5);(4*w_case,19*h_case);(10*w_case/3,19*h_case)|] in
			draw_poly parallelogram_inv;
			let trapezoid = [|(10*w_case/3,19*h_case-5);(11*w_case/3,19*h_case-5);(4*w_case,18*h_case);(3*w_case,18*h_case)|] in
			draw_poly trapezoid;	
			let trapezoid_inv = [|(3*w_case,18*h_case-5);(4*w_case,18*h_case-5);(11*w_case/3,17*h_case);(10*w_case/3,17*h_case)|] in
			draw_poly trapezoid_inv;
	(*choix des couleurs*)
		moveto w_case (24*h_case);
		set_line_width 1;
		draw_string "Color :";
			set_color black;
			fill_rect w_case (23*h_case) w_case (h_case-5);
			set_color magenta;
			fill_rect w_case (22*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (22*h_case) w_case (h_case-5);
			set_color blue;
			fill_rect w_case (21*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (21*h_case) w_case (h_case-5);
			set_color cyan;
			fill_rect w_case (20*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (20*h_case) w_case (h_case-5);
			set_color green;
			fill_rect w_case (19*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (19*h_case) w_case (h_case-5);
			set_color yellow;
			fill_rect w_case (18*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (18*h_case) w_case (h_case-5);
			set_color red;
			fill_rect w_case (17*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (17*h_case) w_case (h_case-5);
			set_color white;
			fill_rect w_case (16*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (16*h_case) w_case (h_case-5);;
	
	
(**fonction initialisant la fenêtre lors du choix d'options pour les flèches**)
let init_edge () =
	let h_case = size_y() / 30 in
	let w_case = size_x() / 36 in
	set_color black;
	(*bouton "Edge"*)
		let (w,h) = text_size "Edge" in
		moveto (7*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (7*w_case/2-w) (53*h_case/2-h) (2*w) (2*h);
		draw_string "Edge";
	(*choix de l'épaisseur du trait*)
		moveto (5*w_case) (24*h_case);
		draw_string "Line width :";
		set_line_width 1;
		draw_segments [|(5*w_case,47*h_case/2,6*w_case,47*h_case/2)|];
		set_line_width 2;
		draw_segments [|(5*w_case,45*h_case/2,6*w_case,45*h_case/2)|];
		set_line_width 3;
		draw_segments [|(5*w_case,43*h_case/2,6*w_case,43*h_case/2)|];
		set_line_width 4;
		draw_segments [|(5*w_case,41*h_case/2,6*w_case,41*h_case/2)|];
	(*choix des couleurs*)
		moveto w_case (24*h_case);
		set_line_width 1;
		draw_string "Color :";
			set_color black;
			fill_rect w_case (23*h_case) w_case (h_case-5);
			set_color magenta;
			fill_rect w_case (22*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (22*h_case) w_case (h_case-5);
			set_color blue;
			fill_rect w_case (21*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (21*h_case) w_case (h_case-5);
			set_color cyan;
			fill_rect w_case (20*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (20*h_case) w_case (h_case-5);
			set_color green;
			fill_rect w_case (19*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (19*h_case) w_case (h_case-5);
			set_color yellow;
			fill_rect w_case (18*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (18*h_case) w_case (h_case-5);
			set_color red;
			fill_rect w_case (17*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (17*h_case) w_case (h_case-5);
			set_color white;
			fill_rect w_case (16*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (16*h_case) w_case (h_case-5);;
	
	
(**fonction initialisant la fenêtre lors du choix d'options pour les ports**)
let init_port () =
	let h_case = size_y() / 30 in
	let w_case = size_x() / 36 in
	set_color black;
	(*bouton "Port"*)
		let (w,h) = text_size "Port" in
		moveto (11*w_case/2-w/2) (53*h_case/2-h/2);
		draw_rect (11*w_case/2-w) (53*h_case/2-h) (2*w) (2*h);
		draw_string "Port";
	(*choix de l'épaisseur du trait*)
		moveto (5*w_case) (24*h_case);
		draw_string "Line width :";
		set_line_width 1;
		draw_segments [|(5*w_case,47*h_case/2,6*w_case,47*h_case/2)|];
		set_line_width 2;
		draw_segments [|(5*w_case,45*h_case/2,6*w_case,45*h_case/2)|];
		set_line_width 3;
		draw_segments [|(5*w_case,43*h_case/2,6*w_case,43*h_case/2)|];
		set_line_width 4;
		draw_segments [|(5*w_case,41*h_case/2,6*w_case,41*h_case/2)|];
	(*choix des couleurs*)
		moveto w_case (24*h_case);
		set_line_width 1;
		draw_string "Color :";
			set_color black;
			fill_rect w_case (23*h_case) w_case (h_case-5);
			set_color magenta;
			fill_rect w_case (22*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (22*h_case) w_case (h_case-5);
			set_color blue;
			fill_rect w_case (21*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (21*h_case) w_case (h_case-5);
			set_color cyan;
			fill_rect w_case (20*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (20*h_case) w_case (h_case-5);
			set_color green;
			fill_rect w_case (19*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (19*h_case) w_case (h_case-5);
			set_color yellow;
			fill_rect w_case (18*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (18*h_case) w_case (h_case-5);
			set_color red;
			fill_rect w_case (17*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (17*h_case) w_case (h_case-5);
			set_color white;
			fill_rect w_case (16*h_case) w_case (h_case-5);
				set_color black;
				draw_rect w_case (16*h_case) w_case (h_case-5);;
	
	
(**fonction affichant l'écran d'aide**)
let show_help () =
	let w_case = size_x()/36 in
	let h_case = size_y()/30 in
	set_color blue;
	moveto (3*w_case) (29*h_case);
	draw_string "How to ...";
	moveto w_case (28*h_case);
	draw_string "... change vertex shape";
	set_color black;
		moveto w_case (28*h_case-13);
		draw_string "- click on button 'Vertex'";
		moveto w_case (28*h_case-26);
		draw_string "- click on a vertex";
		moveto w_case (28*h_case-39);
		draw_string "- choose the shape";
	set_color blue;
	moveto w_case (26*h_case);
	draw_string "... change vertex color";
	set_color black;
		moveto w_case (26*h_case-13);
		draw_string "- click on button 'Vertex'";
		moveto w_case (26*h_case-26);
		draw_string "- click on a vertex";
		moveto w_case (26*h_case-39);
		draw_string "- choose the color";
	set_color blue;
	moveto w_case (24*h_case);
	draw_string "... change edge line width";
	set_color black;
		moveto w_case (24*h_case-13);
		draw_string "- click on button 'Edge'";
		moveto w_case (24*h_case-26);
		draw_string "- click on a vertex";
		moveto w_case (24*h_case-39);
		draw_string "- choose the width";
	set_color blue;
	moveto w_case (22*h_case);
	draw_string "... change edge color";
	set_color black;
		moveto w_case (22*h_case-13);
		draw_string "- click on button 'Edge'";
		moveto w_case (22*h_case-26);
		draw_string "- click on a vertex";
		moveto w_case (22*h_case-39);
		draw_string "- choose the color";
	set_color blue;
	moveto w_case (20*h_case);
	draw_string "... change port line width";
	set_color black;
		moveto w_case (20*h_case-13);
		draw_string "- click on button 'Port'";
		moveto w_case (20*h_case-26);
		draw_string "- click on the vertex with the port";
		moveto w_case (20*h_case-39);
		draw_string "- choose the width";
	set_color blue;
	moveto w_case (18*h_case);
	draw_string "... change port color";
	set_color black;
		moveto w_case (18*h_case-13);
		draw_string "- click on button 'Port'";
		moveto w_case (18*h_case-26);
		draw_string "- click on the vertex with the port";
		moveto w_case (18*h_case-39);
		draw_string "- choose the color";
	set_color blue;
	moveto w_case (16*h_case);
	draw_string "... move a vertex";
	set_color black;
		moveto w_case (16*h_case-13);
		draw_string "- click on a vertex";
		moveto w_case (16*h_case-26);
		draw_string "- drag it where you want";
	set_color blue;
	moveto w_case (14*h_case);
	draw_string "... add avertex";
	set_color black;
		moveto w_case (14*h_case-13);
		draw_string "- click on button 'Add vertex'";
		moveto w_case (14*h_case-26);
		draw_string "- enter a name";
		moveto w_case (14*h_case-39);
		draw_string "- click on button 'Add vertex'";
	set_color blue;
	moveto w_case (12*h_case);
	draw_string "... remove a vertex";
	set_color black;
		moveto w_case (12*h_case-13);
		draw_string "- click on button 'Remove vertex'";
		moveto w_case (12*h_case-26);
		draw_string "- click on a vertex";
	set_color blue;
	moveto w_case (10*h_case);
	draw_string "... link two vertices";
	set_color black;
		moveto w_case (10*h_case-13);
		draw_string "- click on 'Link vertex'";
		moveto w_case (10*h_case-26);
		draw_string "- click on a vertex";
		moveto w_case (10*h_case-39);
		draw_string "- drag the mouse to another vertex";
	set_color blue;
	moveto w_case (8*h_case);
	draw_string "... add ports";
	set_color black;
		moveto w_case (8*h_case-13);
		draw_string "- click on button 'Add port'";
		moveto w_case (8*h_case-26);
		draw_string "- enter a name";
		moveto w_case (8*h_case-39);
		draw_string "- click on button 'Add port'";
		moveto w_case (8*h_case-52);
		draw_string "- click on a vertex";
		moveto w_case (8*h_case-65);
		draw_string "- drag the mouse to another vertex";
	set_color blue;
	moveto w_case (6*h_case-26);
	draw_string "... have a good graph layout";
	set_color black;
		moveto w_case (6*h_case-39);
		draw_string "- click on button 'Layout'";
	set_color blue;
	moveto w_case (4*h_case-13);
	draw_string "... correct a graphic problem";
	set_color black;
		moveto w_case (4*h_case-26);
		draw_string "- click on button 'Reset'";
	set_color blue;
	moveto (7*w_case/2 - fst(text_size"(*click to quit*)")/2) (2*h_case);
	draw_string "(*click to quit*)";
	(*on attend un clique pour quitter la fonction*)
	while not (button_down ()) do
		();
	done;
	(*on attend la fin du clique pour ne pas déclencher un bouton qui serait caché derrière le texte*)
	while button_down () do
		();
	done;;


(**fonction modifiant les coordonnées des noeuds**)
let scale vertices =
	let largeur_max = ref 0 in
	let hauteur_max = ref 0 in
	(*on cherche le maximum en largeur et en hauteur*)
	let max vertex =
		if vertex.abscissa > !largeur_max then
			largeur_max := vertex.abscissa;
		if vertex.ordinate > !hauteur_max then
			hauteur_max := vertex.ordinate;
	in
	List.iter max vertices;
	(*on modifie les coordonnées des noeuds pour qu'ils occupent tout l'espace*)
	let f vertex =
		vertex.abscissa <- 7*size_x()/36 + 29*size_x()/36/(!largeur_max+1)*vertex.abscissa;
		vertex.ordinate <- size_y()/(!hauteur_max+1)*vertex.ordinate;
	in
	List.iter f vertices;;
	
	
(**fonction rétablissant les coordonnées des noeuds**)
let unscale vertices =
	let largeur_min = ref 10000 in
	let hauteur_min = ref 10000 in
	(*on cherche le minimum en largeur et en hauteur*)
	let min vertex =
		if vertex.abscissa < 8*size_x()/36 then
			vertex.abscissa <- 8*size_x()/36;
		if vertex.abscissa < !largeur_min then
			largeur_min := vertex.abscissa;
		if vertex.ordinate < 0 then
			vertex.ordinate <- size_y()/30;
		if vertex.ordinate < !hauteur_min then
			hauteur_min := vertex.ordinate;
	in
	List.iter min vertices;
	largeur_min := !largeur_min - 7*size_x()/36;
	(*on modifie les coordonnées des noeuds pour qu'ils occupent tout l'espace*)
	let f vertex =
		if vertex.abscissa > 7*size_x()/36 then
			vertex.abscissa <- (vertex.abscissa - 7*size_x()/36) / !largeur_min
		else
			vertex.abscissa <- vertex.abscissa / !largeur_min;
		vertex.ordinate <- vertex.ordinate / !hauteur_min;
	in
	List.iter f vertices;;
			

(**fonction dessinant les noeuds**)
let rec draw_vertices vertices =
	set_line_width 1;
	match vertices with
		|[] -> ()
		|h_vertex::t_vertices ->
			(*si on a un empty_vertex, on trace un petit carré et on passe à l'élément suivant*)
			if h_vertex.name = "empty_vertex" then 
				(set_color h_vertex.draw.vertex_color;
				fill_rect (h_vertex.abscissa-2) (h_vertex.ordinate-2) 4 4;
				draw_vertices t_vertices)
			(*sinon, on trace le noeud selon sa forme que l'on adapte au texte*)
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
						draw_vertices t_vertices
					|Ellipse -> 
						fill_ellipse x y (2 * w/3) h;
						set_color black;
						draw_ellipse x y (2 * w/3) h;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices
					|Hexagon -> 
						let hexagon = [|(x-2*w/3,y);(x-w/2,y+h);(x+w/2,y+h);(x+2*w/3,y);(x+w/2,y-h);(x-w/2,y-h)|] in
						fill_poly hexagon;
						set_color black;
						draw_poly hexagon;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices
					| Parallelogram -> 
						let parallelogram = [|(x-w/2,y+h);(x+2*w/3,y+h);(x+w/2,y-h);(x-2*w/3,y-h)|] in
						fill_poly parallelogram;
						set_color black;
						draw_poly parallelogram;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices
					| Parallelogram_inv -> 
						let parallelogram_inv = [|(x-2*w/3,y+h);(x+w/2,y+h);(x+2*w/3,y-h);(x-w/2,y-h)|] in
						fill_poly parallelogram_inv;
						set_color black;
						draw_poly parallelogram_inv;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices
					|Trapezoid -> 
						let trapezoid = [|(x-w/2,y+h);(x+w/2,y+h);(x+2*w/3,y-h);(x-2*w/3,y-h)|] in
						fill_poly trapezoid;
						set_color black;
						draw_poly trapezoid;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices
					|Trapezoid_inv -> 
						let trapezoid_inv = [|(x-2*w/3,y+h);(x+2*w/3,y+h);(x+w/2,y-h);(x-w/2,y-h)|] in
						fill_poly trapezoid_inv;
						set_color black;
						draw_poly trapezoid_inv;
						moveto (x - w/2) (y - h/2);
						draw_string h_vertex.name;
						draw_vertices t_vertices;;
					
					
(**fonction dessinant les flèches**)				
let rec draw_edges vertices =
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
					draw_edges t_vertices
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
				draw_edges t_vertices)
			(*sinon, on s'intéresse aux successeurs de l'élément actuel*)			
			else
				match h_vertex.next_vertices with
					(*s'il n'a pas de successeur, on ne trace rien et on passe à l'élément suivant*)
					|[] -> draw_edges t_vertices
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
							draw_edges (new_vertex::t_vertices) );;

				
(**fonction dessinant et reliant les ports**)
let rec draw_ports ports vertices=
	match ports with
		|[] -> ()
		|h_port::t_ports ->
			(*pour se faciliter le dessin par la suite, on ne prend que les out_port*)
			if h_port.io = "in" then draw_ports (t_ports@[h_port]) vertices
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
						draw_ports otherports vertices
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
						draw_ports otherports vertices );;
	

(**fonction permettant de remettre le graphe à zéro**)
let reset ports vertices =
	unscale vertices;
	clear_graph ();
	init ();
	scale vertices;
	draw_vertices vertices;
	draw_edges vertices;
	draw_ports ports vertices;;
	
	
(**fonction permettant de savoir sur quel noeud est la souris**)
let rec mouse_on_vertex vertices =
	match vertices with
		(*si la liste est vide, on renvoie un noeud que l'on peut facilement identifier dans les fonctions utilisant mouse_on_vertex*)
		|[] -> 
			let drawing_option = {vertex_shape = Rectangle; vertex_color = black; edge_width = 1; edge_color = white} in
			{name = ""; next_vertices = []; abscissa = 0; ordinate = 0; draw = drawing_option}
		|h_vertex::t_vertices -> 
			let (x, y) = mouse_pos () in
			let (w, h) = text_size h_vertex.name in
			let (xv, yv) = (h_vertex.abscissa, h_vertex.ordinate) in
			match x > xv-2*w/3 && x < xv+2*w/3 && y > yv-h && y < yv+h with
				(*si la souris est sur le noeud, on renvoie ce noeud*)
				|true -> h_vertex
				(*sinon, on passe au noeud suivant*)
				|false -> mouse_on_vertex t_vertices;;


(**fonction permettant de lire le nom entré par l'utilisateur**)
let read_name () =
	(*on affiche le cadre qui permet d'entrer un nom*)
	let w_case = size_x()/36 in
	let h_case = size_y()/30 in
	set_color black;
	set_line_width 1;
	moveto w_case (15*h_case);
	draw_string "Enter a name :";
	draw_rect w_case (15*h_case-30) (5*w_case) 27;
	(*on récupère le nom du nouveau noeud et on l'affiche*)
	let string_of_char = String.make 1 in
	let name = ref "" in
	moveto (w_case+5) (15*h_case-16);
	while not (button_down ()) do
		let (x, _) = current_point () in
		if key_pressed () then (
			let lettre = read_key () in
			name := !name ^ (string_of_char lettre);
			if (x < 6*w_case-5) then
				draw_char lettre
			else
				(moveto (w_case+5) (15*h_case-29);
				draw_char lettre) ) ;
	done;
	(*on efface le cadre qui permettait d'entrer un nom*)
	set_color white;
	fill_rect w_case (15*h_case-30) (5*w_case) (30+h_case);
	(*on attend que le bouton soit relaché*)
	while button_down () do
		();
	done;
	(*on renvoie le nom*)
	!name;;


(**fonction permettant de créer un noeud**)
let create_vertex vertices =
	(*on récupère le nom du noeud*)
	let name = read_name () in
	if name <> "" then (
		let (w, h) = text_size name in
		(*on crée le nouvau noeud*)
		let drawing_option = {vertex_shape = Rectangle; vertex_color = yellow; edge_width = 1; edge_color = green} in
		let new_vertex = {name = name; next_vertices = []; abscissa = size_x()-2*w; ordinate = size_y()-2*h; draw = drawing_option} in
		(*on trace le nouveau noeud*)
		vertices := new_vertex::!vertices;
		draw_vertices !vertices );;
	

(**fonction permettant de supprimer un noeud**)
let remove_vertex ports vertices =
	(*on attend un clique sur un noeud*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud sur lequel on vient de cliquer*)
	let vertex = mouse_on_vertex !vertices in
	if vertex.name <> "" then (
		(*on supprimme les flèches qui avaient un lien avec ce noeud*)
		let f vertx =
			vertx.next_vertices <- List.filter (fun v -> not(v == vertex)) vertx.next_vertices
		in
		ignore(List.map f !vertices);
		(*on supprimme les ports qui avaient un lien avec ce noeud*)
		let port_list = List.filter (fun p -> p.vertex_name = vertex.name) !ports in
		if port_list <> [] then
			(let g port =
				ports := List.filter (fun p -> p.port_name <> port.port_name) !ports
			in
			ignore(List.map g port_list));
		(*on retire ce noeud de la liste des noeuds*)
		vertices := List.filter (fun v -> not (v == vertex)) !vertices;
		(*on efface le graphe à l'écran*)
		set_color white;
		fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-8) (size_y()-8);
		(*on trace le nouveau graphe*)
		draw_vertices !vertices;
		draw_edges !vertices;
		draw_ports !ports !vertices );;


(**fonction permettant de relier deux noeuds**)
let link_vertex vertices =
	(*on attend un clique sur un noeud*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud sur lequel on vient de cliquer*)
	let vertex1 = mouse_on_vertex vertices in
	(*on attend que le bouton soit relacher*)
	while button_down () do
		();
	done;
	(*on récupère le deuxième noeud*)
	let vertex2 = mouse_on_vertex vertices in
	(*on ajoute le second noeud dans la liste des successeurs du premier*)
	vertex1.next_vertices <- vertex2::vertex1.next_vertices;
	(*on trace les flèches entre les noeuds*)
	draw_edges vertices;;
	
	
(**fonction permettant d'ajouter des ports**)
let add_port ports vertices =
	(*on récupère le nom du noeud*)
	let name = read_name () in
	(*on attend un clique sur un noeud*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud sur lequel on vient de cliquer*)
	let vertex1 = mouse_on_vertex !vertices in
	if vertex1.name <> "" then (
		(*on attend que le bouton soit relacher*)
		while button_down () do
			();
		done;
		(*on récupère le deuxième noeud*)
		let vertex2 = mouse_on_vertex !vertices in
		if vertex2.name <> "" then (
			(*on crée les deux nouveaux ports*)
			let port1 = {port_name = name; vertex_name = vertex1.name; io = "out"; width = 1; color = cyan} in
			let port2 = {port_name = name; vertex_name = vertex2.name; io = "in"; width = 1; color = cyan} in
			(*on trace les nouveaux ports*)
			ports := port1::port2::!ports;
			draw_ports !ports !vertices) );;
	

(**fonction permettant d'améliorer l'affichage du graphe**)
let layout ports vertices =
	(*on convertit les vertex en liste de lien*)
	let link_list = ref [] in
	let rec f v =
		match v.next_vertices with
		|[] -> ()
		|h_vertex::t_vertices ->
			if v.name <> "empty_vertex" then (
				if h_vertex.name <> "empty_vertex" then
					(link_list := (v.name, h_vertex.name)::!link_list;
					let new_v = {name = v.name; next_vertices = t_vertices; abscissa = v.abscissa; ordinate = v.ordinate; draw = v.draw} in
					f new_v)
				else
					(let new_v = {name = v.name; next_vertices = (h_vertex.next_vertices)@t_vertices; abscissa = v.abscissa; ordinate = v.ordinate; draw = v.draw} in
					f new_v) )
	in
	List.iter f !vertices;
	(*on appelle la fonction graph_layout*)
	vertices := graph_layout !link_list;
	(*on met à l'échelle les coordonnées des noeuds*)
	scale !vertices;
	(*on trace les noeuds, les flèches et les ports*)
	set_color white;
	fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-8) (size_y()-8);
	draw_vertices !vertices;
	draw_edges !vertices;
	draw_ports !ports !vertices;;


(**fonction gérant le chois d'option pour les noeuds**)
let vertex_option ports vertices =
	let w_case = size_x() / 36 in
	let h_case = size_y() / 30 in
	(*on affiche la bonne fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init_vertex ();
	(*on attend un clique sur un bouton*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud surlequel on a cliqué*)
	let vertex = mouse_on_vertex vertices in
	if vertex.name <> "" then (			
		(*on attend que le bouton soit relaché*)
		while button_down () do
			();
		done;
		(*on attend un nouveau clique sur un bouton*)
		while not (button_down ()) do
			();
		done;
		let (x, y) = mouse_pos () in
		(*choix de la forme des neouds*)
		if x > 3*w_case && x < 4*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then 
				vertex.draw.vertex_shape <- Rectangle;
			if y > 22*h_case && y < 23*h_case-5 then 
				vertex.draw.vertex_shape <- Ellipse;
			if y > 21*h_case && y < 22*h_case-5 then
				vertex.draw.vertex_shape <- Hexagon;
			if y > 20*h_case && y < 21*h_case-5 then 
				vertex.draw.vertex_shape <- Parallelogram;
			if y > 19*h_case && y < 20*h_case-5 then 
				vertex.draw.vertex_shape <- Parallelogram_inv;
			if y > 18*h_case && y < 19*h_case-5 then 
				vertex.draw.vertex_shape <- Trapezoid;
			if y > 17*h_case && y < 18*h_case-5 then 
				vertex.draw.vertex_shape <- Trapezoid_inv );
		(*choix de la couleur des neouds*)
		if x > w_case && x < 2*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then 
				vertex.draw.vertex_color <- black;
			if y > 22*h_case && y < 23*h_case-5 then 
				vertex.draw.vertex_color <- magenta;
			if y > 21*h_case && y < 22*h_case-5 then 
				vertex.draw.vertex_color <- blue;
			if y > 20*h_case && y < 21*h_case-5 then 
				vertex.draw.vertex_color <- cyan;
			if y > 19*h_case && y < 20*h_case-5 then 
				vertex.draw.vertex_color <- green;
			if y > 18*h_case && y < 19*h_case-5 then 
				vertex.draw.vertex_color <- yellow;
			if y > 17*h_case && y < 18*h_case-5 then 
				vertex.draw.vertex_color <- red;
			if y > 16*h_case && y < 17*h_case-5 then 
				vertex.draw.vertex_color <- white );
		set_color white;
		fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-6) (size_y()-6);
		draw_vertices vertices;
		draw_edges vertices;
		if ports <> [] then
			draw_ports ports vertices );
	(*on rétablit la fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init ();;


(**fonction gérant le chois d'option pour les flèches**)
let edge_option ports vertices =
	let w_case = size_x() / 36 in
	let h_case = size_y() / 30 in
	(*on affiche la bonne fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init_edge ();
	(*on attend un clique sur un bouton*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud surlequel on a cliqué*)
	let vertex = mouse_on_vertex vertices in
	if vertex.name <> "" then (
		(*on cherche à récupérer tous les empty_vertex lié à ce noeud*)
		let vertex_list = ref [vertex] in
		let rec f vert =
			let empty_vertex_list = List.filter (fun v -> v.name = "empty_vertex") vert.next_vertices in
			if (empty_vertex_list <> []) then
				(vertex_list := empty_vertex_list @ (!vertex_list);
				ignore(List.map f empty_vertex_list))
		in
		f vertex;	
		(*on attend que le bouton soit relaché*)
		while button_down () do
			();
		done;
		(*on attend un nouveau clique sur un bouton*)
		while not (button_down ()) do
			();
		done;
		let (x, y) = mouse_pos () in
		(*choix de l'épaisseur des traits*)
		if x > 5*w_case && x < 6*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then
				ignore(List.map (fun v -> v.draw.edge_width <- 1) !vertex_list);
			if y > 22*h_case && y < 23*h_case-5 then
				ignore(List.map (fun v -> v.draw.edge_width <- 2) !vertex_list);
			if y > 21*h_case && y < 22*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_width <- 3) !vertex_list);
			if y > 20*h_case && y < 21*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_width <- 4) !vertex_list) );
		(*choix de la couleur des flèches*)
		if x > w_case && x < 2*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- black) !vertex_list);
			if y > 22*h_case && y < 23*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- magenta) !vertex_list);
			if y > 21*h_case && y < 22*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- blue) !vertex_list);
			if y > 20*h_case && y < 21*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- cyan) !vertex_list);
			if y > 19*h_case && y < 20*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- green) !vertex_list);
			if y > 18*h_case && y < 19*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- yellow) !vertex_list);
			if y > 17*h_case && y < 18*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- red) !vertex_list);
			if y > 16*h_case && y < 17*h_case-5 then 
				ignore(List.map (fun v -> v.draw.edge_color <- white) !vertex_list) );
		set_color white;
		fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-6) (size_y()-6);
		draw_vertices vertices;
		draw_edges vertices;
		if ports <> [] then
			draw_ports ports vertices );
	(*on rétablit la fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init ();;


(**fonction gérant le chois d'option pour les ports**)
let port_option ports vertices =
	let w_case = size_x() / 36 in
	let h_case = size_y() / 30 in
	(*on affiche la bonne fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init_port ();
	(*on attend un clique sur un bouton*)
	while not (button_down ()) do
		();
	done;
	(*on récupère le noeud surlequel on a cliqué*)
	let vertex = mouse_on_vertex vertices in
	if vertex.name <> "" then (
		(*on récupère les ports qui sont sur ce noeud*)
		let port_list1 = List.filter (fun p -> p.vertex_name = vertex.name) ports in
		(*on cherche les ports associés aux ports de ce noeud*)
		let port_list2 = ref [] in
		let f port =
			port_list2 := (List.find (fun p -> p.port_name = port.port_name && p <> port) ports)::!port_list2;
		in
		List.iter f port_list1;
		(*on rassemble tous les ports à modifier dans la même liste*)
		let port_list = port_list1@(!port_list2) in
		(*on attend que le bouton soit relaché*)
		while button_down () do
			();
		done;
		(*on attend un nouveau clique sur un bouton*)
		while not (button_down ()) do
			();
		done;
		let (x, y) = mouse_pos () in
		(*choix de l'épaisseur des traits*)
		if x > 5*w_case && x < 6*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then
				ignore(List.map (fun p -> p.width <- 1) port_list);
			if y > 22*h_case && y < 23*h_case-5 then
				ignore(List.map (fun p -> p.width <- 2) port_list);
			if y > 21*h_case && y < 22*h_case-5 then 
				ignore(List.map (fun p -> p.width <- 3) port_list);
			if y > 20*h_case && y < 21*h_case-5 then 
				ignore(List.map (fun p -> p.width <- 4) port_list) );
		(*choix de la couleur des ports*)
		if x > w_case && x < 2*w_case then (
			if y > 23*h_case && y < 24*h_case-5 then 
				ignore(List.map (fun p -> p.color <- black) port_list);
			if y > 22*h_case && y < 23*h_case-5 then 
				ignore(List.map (fun p -> p.color <- magenta) port_list);
			if y > 21*h_case && y < 22*h_case-5 then 
				ignore(List.map (fun p -> p.color <- blue) port_list);
			if y > 20*h_case && y < 21*h_case-5 then 
				ignore(List.map (fun p -> p.color <- cyan) port_list);
			if y > 19*h_case && y < 20*h_case-5 then 
				ignore(List.map (fun p -> p.color <- green) port_list);
			if y > 18*h_case && y < 19*h_case-5 then 
				ignore(List.map (fun p -> p.color <- yellow) port_list);
			if y > 17*h_case && y < 18*h_case-5 then 
				ignore(List.map (fun p -> p.color <- red) port_list);
			if y > 16*h_case && y < 17*h_case-5 then 
				ignore(List.map (fun p -> p.color <- white) port_list) );
		set_color white;
		fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-6) (size_y()-6);
		draw_vertices vertices;
		draw_edges vertices;
		draw_ports ports vertices);
	(*on rétablit la fenêtre*)
	set_color white;
	fill_rect (w_case/2) (15*h_case) (13*w_case/2-5) (13*h_case-5);
	init ();;


(**fonction gérant l'appuie sur un boutton**)
let button_pressed title save_number ports vertices =
	let h_case = size_y() / 30 in
	let w_case = size_x() / 6 / 6 in
	(*on teste s'il y a eu un clique*)
	if button_down () then (
		let (x, y) = mouse_pos () in
			(*bouton "Add vertex"*)
			let (wav, hav) = text_size "Add vertex" in
			if x > 7*w_case/2 - 2*wav/3 && x < 7*w_case/2 + 2*wav/3 && y > 13*h_case - hav && y < 13*h_case + hav then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				create_vertex vertices);
			(*bouton "Remove vertex"*)
			let (wre, hre) = text_size "Remove vertex" in
			if x > 7*w_case/2 - 2*wre/3 && x < 7*w_case/2 + 2*wre/3 && y > 11*h_case - hre && y < 11*h_case + hre then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				remove_vertex ports vertices);
			(*bouton "Link vertex"*)
			let (wl, hl) = text_size "Link vertex" in
			if x > 7*w_case/2 - 2*wl/3 && x < 7*w_case/2 + 2*wl/3 && y > 9*h_case - hl && y < 9*h_case + hl then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				link_vertex !vertices);
			(*bouton "Add port"*)
			let (wap, hap) = text_size "Add port" in
			if x > 7*w_case/2 - 2*wap/3 && x < 7*w_case/2 + 2*wap/3 && y > 7*h_case - hap && y < 7*h_case + hap then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				add_port ports vertices);
			(*bouton "Help"*)
			let (wh, hh) = text_size "Help" in
			if x > 5*w_case/2 - wh && x < 5*w_case/2 + wh && y > 9*h_case/2 - hh && y < 9*h_case/2 + hh then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				set_color white;
				fill_rect (w_case/2) (2*h_case) (13*w_case/2-5) (28*h_case-5);
				show_help ();
				set_color white;
				fill_rect (w_case/2) (2*h_case) (13*w_case/2-5) (28*h_case-5);
				init () );
			(*bouton "Layout"*)
			let (wso, hso) = text_size "Layout" in
			if x > 9*w_case/2 - 2*wso/3 && x < 9*w_case/2 + 2*wso/3 && y > 9*h_case/2 - hso && y < 9*h_case/2 + hso then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				layout ports vertices);
			(*bouton "Save"*)
			let (wsa, hsa) = text_size "Save" in
			if x > 5*w_case/2 - wsa && x < 5*w_case/2 + wsa && y > 5*h_case/2 - hsa && y < 5*h_case/2 + hsa then (
				(*on attend que le bouton soit relacher pour ne pas générer plusieurs pdf*)
				while button_down () do
					 ();
				done;
				save_pdf (title^" "^string_of_int !save_number) (size_x()) (size_y()) !ports !vertices;
				(*on incrémente le numéro de sauvegarde pour ne pas écraser par la suite le pdf*)
				save_number := !save_number + 1 );
			(*bouton "Quit"*)
			let (wq, hq) = text_size "Quit" in
			if x > 9*w_case/2 - wq && x < 9*w_case/2 + wq && y > 5*h_case/2 - hq && y < 5*h_case/2 + hq then
					raise End;
			(*bouton "Reset"*)
			let (wr, hr) = text_size "Reset" in
			if x > 0 && x < 2*wr && y > 0 && y < 2*hr then
				(*on attend que le bouton soit relacher pour appeler la fonction*)
				(while button_down () do
					();
				done;
				reset !ports !vertices);
			(*boutons gérant les options de dessin des noeuds*)
				(*bouton "Vertex"*)
				let (wv, hv) = text_size "Vertex" in
				if x > 3*w_case/2-2*wv/3 && x < 3*w_case/2+2*wv/3 && y > 53*h_case/2-hv && y < 53*h_case/2+hv then
					(*on attend que le bouton soit relacher pour appeler la fonction*)
					(while button_down () do
						();
					done;
					vertex_option !ports !vertices);
				(*bouton "Edge"*)
				let (we, he) = text_size "Edge" in
				if x > 7*w_case/2-we && x < 7*w_case/2+we && y > 53*h_case/2-he && y < 53*h_case/2+he then
					(*on attend que le bouton soit relacher pour appeler la fonction*)
					(while button_down () do
						();
					done;
					edge_option !ports !vertices);
				(*bouton "Port"*)
				let (wp, hp) = text_size "Port" in
				if x > 11*w_case/2-wp && x < 11*w_case/2+wp && y > 53*h_case/2-hp && y < 53*h_case/2+hp then
					(*on attend que le bouton soit relacher pour appeler la fonction*)
					(while button_down () do
						();
					done;
					port_option !ports !vertices) );;


(**fonction permettant de déplacer un noeud**)
let move_vertex ports vertices =
	(*fonction gérant le déplacement d'un noeud*)
	let f v =
		if button_down () then (
			while button_down () do
				let (x, y) = mouse_pos () in
				(*on met à jour les coordonnées du noeud qu'on déplace*)
				v.abscissa <- x;
				v.ordinate <- y;
			done;
			(*on efface le graphe à l'écran et on trace le nouveau*)
			set_color white;
			fill_rect (7*size_x()/36+3) 3 (29*size_x()/36-8) (size_y()-8);
			draw_vertices vertices;
			draw_edges vertices;
			if ports <> [] then
				draw_ports ports vertices )
	in
	(*on trouve le noeud à déplacer*)
	let vertex = mouse_on_vertex vertices in
	(*on effectue le déplacement*)
	if vertex.name <> "" then
		f vertex;;


(**fonction gérant l'interface**)
let interface title ports vertices =
	(*on initialise les références qui seront envoyer aux fonctions*)
	let save_number = ref 1 in
	let continue = ref 1 in
	while !continue = 1 do
		(*selon la position de la souris sur l'écran, on regarde quelles fonctions appeler*)
		let (x, _) = mouse_pos () in
		(*si la souris est à gauche, c'est que l'on veut appuyer sur un bouton*)
		if x < 7*size_x()/36 then
			try button_pressed title save_number ports vertices with
				|End -> continue := 0; 
					close_graph ();
		(*sinon, si la souris est à droite, c'est que l'on veut interagir avec le graphe*)
		else if x > 7*size_x()/36 then
			(if button_down () then
				move_vertex !ports !vertices)
	done;;


(**fonction principale qui appelle les autres**)
let graph_draw title ?(width = 1440) ?(height = 900) port_list link_list =
	let vertices = ref (vertex_of_graph link_list) in
	let ports = ref port_list in
	(*on ouvre une fenêtre*)
	open_window title width height;
	(*on initialise la fenêtre*)
	init ();
	(*on affiche le graph*)
	layout ports vertices;
	(*on appelle la fonction gérant les interactions*)
	interface title ports vertices;;
