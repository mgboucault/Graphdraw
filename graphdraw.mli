(*Copyright 2014 Marcel-Georges BOUCAULT & Frank WANG*)


open Graphics
open Types


exception End


(**fonction ouvrant la fenêtre**)
val open_window : string -> int -> int -> unit


(**fonction initialisant la fenêtre**)
val init : unit -> unit


(**fonction initialisant la fenêtre lors du choix d'options pour les noeuds**)
val init_vertex : unit -> unit


(**fonction initialisant la fenêtre lors du choix d'options pour les flèches**)
val init_edge : unit -> unit


(**fonction initialisant la fenêtre lors du choix d'options pour les ports**)
val init_port : unit -> unit


(**fonction affichant l'écran d'aide**)
val show_help : unit -> unit


(**fonction modifiant les coordonnées des noeuds**)
val scale : vertex list -> unit


(**fonction rétablissant les coordonnées des noeuds**)
val unscale : vertex list -> unit


(**fonction permettant de remettre le graphe à zéro**)
val reset : port list -> vertex list -> unit


(**fonction permettant de savoir sur quel noeud est la souris**)
val mouse_on_vertex : vertex list -> vertex


(**fonction permettant de lire le nom entré par l'utilisateur**)
val read_name : unit -> string


(**fonction permettant de créer un noeud**)
val create_vertex : vertex list ref -> unit


(**fonction permettant de supprimer un noeud**)
val remove_vertex : port list ref -> vertex list ref -> unit


(**fonction permettant de relier deux noeuds**)
val link_vertex : vertex list -> unit


(**fonction permettant d'ajouter des ports**)
val add_port : port list ref -> vertex list ref -> unit


(**fonction permettant d'améliorer l'affichage du graphe**)
val layout : port list ref -> vertex list ref -> unit


(**fonction gérant le chois d'option pour les noeuds**)
val vertex_option : port list -> vertex list -> unit


(**fonction gérant le chois d'option pour les flèches**)
val edge_option :  port list -> vertex list -> unit


(**fonction gérant le chois d'option pour les ports**)
val port_option : port list -> vertex list -> unit


(**fonction gérant l'appuie sur un boutton**)
val button_pressed : string -> int ref -> port list ref -> vertex list ref -> unit
  

(**fonction permettant de déplacer un noeud**)
val move_vertex : port list -> vertex list -> unit
                             
                             
(**fonction gérant l'interface**)
val interface : string -> port list ref -> vertex list ref -> unit


(**fonction principale qui appelle les autres**)
val graph_draw : string -> ?width:int -> ?height:int -> port list -> (string * string) list -> unit
