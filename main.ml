(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphdraw;;

let () =

	(* Entrez le titre du graphe *)
	let graph_title = "Graph Drawing" in

	(* Entrez la liste des ports présents dans votre graphe *)
	(* exemple : voici la syntaxe pour définir un port *)
	(* let port = {port_name = "nom du port"; 
					vertex_name = "nom du noeud surlequel se trouve le port"; 
					io = "in"/"out"; width = un entier entre 1 et 4; 
					color = black/magenta/blue/cyan/green/yellow/red/white} *)
	(* Attention : pour chaque port "in", il doit y avoir un port "out" possédant le même nom *)
	(* Attention : pour chaque port, le champ vertex_name doit être le nom d'un noeud présent dans le graphe *)
	(* Note : il est possible de rajouter les ports ultérieurement *)
	let port_list = [] in

	(* Entrez votre graphe sous forme d'une liste de lien *)
	(* exemple : si votre graphe contient un arc orienté de a vers b, il faut mettre le couple ("a", "b") dans la liste de lien *)
	(* Note : il est possible de rajouter des noeuds ultérieurement *)
	let graph = [("a","b");("b","c");("c","a");("d","e");("e","f");("f","d");("c","d")] in


	graph_draw graph_title port_list graph ;;
