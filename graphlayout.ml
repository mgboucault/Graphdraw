open Graphdrawpdf;;

                                                              (** Sugiyama algorithm **)

                                                              (** I  : Cycle removal **)

                                                              (** I-a:  Préliminaire **)


(** fonction qui enleve les occurences multiples de la liste l *) 
let no_occ l=

	let rec no_occ' l l' =(
	  match l with 
	    |[]->l'
	    |h::t -> if (List.mem h t ) then no_occ' t l'
	                             		else no_occ' t (l'@[h])
  )in
  (*on enlève les occurence par la droite *)
  no_occ' (List.rev l ) []
;; 

(**Permet de calculer le max d'une liste d'entier*)
  let rec list_max l=
    let rec list_max' l m=
     match l with 
      |[]->m
      |h::t->list_max' t (max h m)
    in
    
    match l with 
    |[]->0
    |h::t->list_max' t h
;;
  
(** fonction qui enleve les occurences multiples pour une liste non ordonnée*)
let rec no_occ_eq l =
    
  (* égalité pour des listes non ordonnées*)
  let rec liste_eq l1 l2=(

  	let rec liste_eq_tmp l1 l2 =(
	 		match (l1,l2) with
	 		  | ([],_)-> true
	 		  | (h1::t1,l) -> if not(List.mem h1 l) then false
	 										  else liste_eq_tmp t1 l2 
	 	)in

	   if (List.length l1) < (List.length l2) then liste_eq_tmp l2 l1
	 																			   else liste_eq_tmp l1 l2
	 )in
	 
  (* eq_is_in a l <=> a appartient à l pour listes non ordonnées*)
  let rec eq_is_in a l=(

	  match l with
	    |[]->false
	    |h::t -> if (liste_eq a h) then true
	             else eq_is_in a t
  )in

  match l with 
    |[]->[]
    |h::t -> if (eq_is_in h t ) then no_occ_eq t
                             else [h]@(no_occ_eq t)
;; 

(** Création de la liste des noeuds à partir du graph *)
let list_noeud liste_lien=  let tmp = List.split liste_lien in no_occ((fst tmp)@(snd tmp));;

                                                                                  (** I-b: Modélisation **)


(** conversion d'un graph en fonction: entrée liste d'éléments
suivant graph ["a";"b"] = ["b";"d";"e";"f"] *)
let suivant liste_lien list_noeud=

  (* conversion d'un graph en fonction: entrée 1 élément
  f_lien graph "b" = ["d";"e";"f"] *)
  let f_lien liste_lien element =(
    
    let rec f_lien' liste_lien element list_out=(
      match liste_lien with
        |[] -> list_out
        |h::t -> if ((fst h)=element)then f_lien' t element ((snd h)::list_out)
                                     else f_lien' t element list_out
    )in
    f_lien' liste_lien element []
  )in

  let rec suivant' liste_lien list_noeud list_out=
    match list_noeud with
      |[]   -> list_out
      |h::t -> suivant' liste_lien t ((f_lien liste_lien h)@list_out)
  in
  suivant' liste_lien list_noeud []
;;

(** Création d'une liste de liste à partir de la première liste en ajoutant un élément de la deuxième
ex:
completer [1;2;3] [4;5] = [ [4;1;2;3] ; [5;1;2;3] ]  
*)
let completer l1 l2=

	let rec completer_tmp l1 l2 l_out=
	  match l2 with
	    |[]   -> l_out
	    |h::t -> completer_tmp l1 t ([h::l1]@l_out)
	in

	if l2=[] then [l1]
	else completer_tmp l1 l2 []
;;

(** affirer tout les chemains / cycle qui parte d'un noeud (origine) *)
let chemin liste_lien origine =
	
	(*Permet de placer le noeud suivant d'un chemin pour une liste de chemain
	  s'il y a plusieurs noeuds fils, la fonction va créer deux chemain avec les deux noeuds fils*)
	let rec chemin0 liste_lien liste origine =(
	  match liste with
	    |[]->[]
	    |hl::tl -> match hl with
	                 |h::t -> if List.mem h t then [h::t]@(chemin0 liste_lien tl origine)   (*On vérifie si on n'a pas un cycle , si oui on arrete d'ajouter des éléments*)
					                  else (completer (h::t) (suivant liste_lien [h]) )@(chemin0 liste_lien tl origine)
	                 |[]->[]
	)in

	(*permet de trouver les chemins partant d'origine en verifiant les cas particuliers 
	  avec utilisation de chemain recursivement *)
	let rec chemin1 liste_lien liste origine =
  	match (liste,chemin0 liste_lien liste origine ) with
	  |([], _)->[]
	  |(l1,l2)-> if l1=l2 then l1
	                      else chemin1 liste_lien l2 origine
	in

	chemin1 liste_lien [[origine]] origine
;;

(** Chercher tous les cycle du graph déduit de liste_lien*)
let list_cycle liste_lien = 

  (* Permet de récuperer seulement les cycles commencent par "origine" dans "liste_chemin"*)
  let rec trier_cycle liste_chemin origine=
    match liste_chemin with
      |[]->[]
      |h_chemin::t_chemin -> match h_chemin with 
                   |[]->[]
                   |origine::[]-> trier_cycle t_chemin origine
                   |h::t -> if (h=origine)then ([h::t])@(trier_cycle t_chemin origine) 
                                         else trier_cycle t_chemin origine
  in

  (*Permet de récuperer tout les cycles a partir de la liste des noms des noeuds *)
	let rec chercher_cycle_tmp liste_lien liste_nom = (
	  match liste_nom with
	    |[]-> []
	    |h::t -> (trier_cycle (chemin liste_lien h) h)@(chercher_cycle_tmp liste_lien t)
	)in

	no_occ_eq (chercher_cycle_tmp liste_lien (list_noeud liste_lien))
;;


                                      (** I-c: Greedy algorithm ( trouver les flèches + choix/modification des flèches) **)

(**Permet de trouver les flèches qui forment les cycles*)
let rec trouver_fleches liste_cycle=

  (*Permet de recupérer les flèches de chaque cycle*)
	let rec fleche_cycle liste_cycle =
		match liste_cycle with 
			|[]->[]
			|[h]->[]
			|h1::(h2::t)->[(h2,h1)]@(fleche_cycle ([h2]@t))
	in
	(*vérification du cas particulier [] *)
	match liste_cycle with
		|[]->[]
		|hl::tl -> (fleche_cycle hl)@(trouver_fleches tl)
;;

(** Permet de donner pour chaque flèches le nombre de cycles dans lesquels elle est comprise   
    Cet fonction est l'heuristique à utiliser pour le greedy algotithme                      *)
let occurrence_fleches  liste_lien=
  
  (* fonction qui compte les occurences de a dans la liste l *) 
  let rec nb_occ a l=
    let rec nb_occ' a l n=
      match l with 
        |[]->n
        |h::t -> if a=h then  nb_occ' a t (n+1)
                                 else nb_occ' a t n
    in
    nb_occ' a l 0
  in

  let  remove a l =List.filter (fun x-> (x<>a)) l in

	(*permet de compter les cycles pour la liste de flèches*)
	let rec greedy_cycle_trier liste_fleche l_out=	
		match liste_fleche with
			|[]->l_out
			|(a,b)::t-> greedy_cycle_trier ( remove (a,b) liste_fleche) ((a,b,1+(nb_occ (a,b) t))::l_out)
	in
	
	(*relation d'ordre pour trier les triplets*)
	let ordre = List.fast_sort (fun (a1,b1,n1) (a2,b2,n2) -> (if n2<>n1 then n2-n1 else ((Random.int 3)-1)) ) in
	
	(*Permet de placer ceux avec la meilleure heuristique au début de la liste*)
	ordre (greedy_cycle_trier (trouver_fleches(list_cycle liste_lien)) [] )
;;
	
(**  Permet de retourner des flèches pour rendre le graphe acyclique         
     heuristique: on inverse les flèche qui influencent le plus de cycles  *)
let rec cycle_removal liste_lien =
  (*Permet de modifier un graphe (en liste de lien ) en entrant le couple à inverser*)
  let rec modifier_fleche liste_lien couple=(
  	match (liste_lien,couple) with
  		|([],_)->[]
  		|( h::t , (a,b) ) -> if (a,b)= h then (b,a)::t
  												 else h::(modifier_fleche t couple)
  )in

  (*Tant qu'il reste des cycles on modifie des flèches*)
  let cycle_l = list_cycle liste_lien in
  match List.length cycle_l with
  	|0 -> (liste_lien,[])
  	|_ -> match (occurrence_fleches liste_lien) with
  					|[]         -> (liste_lien,[])
  					|(a,b,n)::t -> let resultat=cycle_removal (modifier_fleche liste_lien (a,b)) in 												
  													(fst resultat , (b,a)::( snd resultat))	
;; 

                                                                          (** II  : Layering **)

                                                                          (** II-a Préliminaire**)


(**Conversion du tableu sous forme regroupé
exemple :
[(a,b);(a,c);(b,d)]->[ (a,[b;c]) ; (b,[d]) ; (c,[]) ; (d,[]) ]
*)
let table graph=
  
  (*Permet de donner la liste des fils du noeud element*)
	let rec table_tmp graph element= 
	  match graph with
	    |[]->[]
	    |(a,b)::t -> if a=element then [b]@(table_tmp t element)
	                              else (table_tmp t element)                
	in

  (*Permet de récuperer le tableau de lien*)
	let rec table_tmp0 graph l= 
	  match l with
	    |[]->[]
	    |a::t -> [(a,table_tmp graph a)]@(table_tmp0 graph t)                   
	in
	
	table_tmp0 graph (list_noeud graph)
;;

(**Conversion graph tab -> liste *)
let list_of_graph tableau_lien=

	(*  permet de dégrouper les lien
	ex : (a,[b;c])  ->  [(a,b);(a,c)]  *)
	let rec list_of_graph_tmp (a,liste_nom) =
	  match liste_nom with
	    |[] -> []
	    |h::t-> [(a,h)]@(list_of_graph_tmp (a,t))
	in

	let rec list_of_graph0 tableau_lien l_out= 
	  match tableau_lien with
	    |[]->l_out
	    |couple ::tl -> if snd couple=[] then list_of_graph0 tl ([(fst couple,fst couple)]@l_out)
	                    else                  list_of_graph0 tl ((list_of_graph_tmp couple)@l_out)
	in
  (*cas particulier*)
	match tableau_lien with
	  |[(a,[])]->[(a,a)]
    |graph -> list_of_graph0 graph []
;;

                                                                  (**II-b Manipulation de graphe**)



(**Permet de modifier la coordinate d'un vertex dans le graph comme liste des positions (liste_position)  
modifier [(a,0,1);(b,2,3)] 1 a 4 = [(a,4,1);(b,2,3)]
modifier [(a,0,1);(b,2,3)] 2 b 4 = [(a,0,1);(b,2,4)]
*)
let rec modifier liste_position coordinate element valeur =
  match liste_position with 
    |[] -> []
    |(a,b,c)::tl -> if a=element then 
                      if coordinate = 1 then (a,valeur,c)::tl
                      else (a,b,valeur)::tl
                    else (a,b,c)::(modifier tl coordinate element valeur)
;;  

(**Permet de déposer tous les bouts d'un graphe (sous forme de liste de lien ) sur un étage du graphe (liste_position) *)
let poser n liste_position tableau_lien = 
	let rec poser' n liste_position tableau_lien liste_bout= 
		match tableau_lien with
			|[]->(liste_position,liste_bout)
			|(element,liste)::t_tableau_lien -> if liste=[] then poser' n (modifier liste_position 2 element n) t_tableau_lien (element::liste_bout)
																											else poser' n (liste_position                     ) t_tableau_lien (         liste_bout)
	in
	poser' n liste_position tableau_lien []
;;

(**fonction qui enleve tout les noeuds de la liste l d'un tableau de lien*)
let enlever_tab tableau_lien l =

  (*Permet d'enlever element d'un tableau de lien*)
	let enlever_tab0 tableau_lien element = 
		List.map 
		(fun (a,liste_nom)-> (a,List.filter (fun nom-> nom<>element ) liste_nom ) ) 
		( List.filter (fun (a,l)-> a<>element ) tableau_lien)
	in
	
	(*enlever les éléments d'une liste *)
	let rec enlever_tab1 tableau_lien liste = 
	      match liste with
	        |[]-> tableau_lien
	        |h::t -> enlever_tab1 (enlever_tab0 tableau_lien h) t
	in

  match tableau_lien with
    |[(a,[])]->[]
    |tableau_lien0-> enlever_tab1 tableau_lien0 l
;;

(** II-c longest path layering **)

(** Définir l'ordonnée de chaque vertex, selon la methode de "longest path layering" 
entree: liste_lien    (sintg1,sting2) list
sortie: liste position   (sting,int1,int2) list 
*)

let layering liste_lien=

	(*Initialisation d'un graph avec avec les noms de vertex*)
  let crea graph =
  
  	let rec crea0 liste =
  	  match liste with
  	    |[] -> []
  	    |h::t -> (h,1,1)::(crea0 t)
  	in  
    	
  	crea0 (list_noeud graph)
  in

	let etage_tab_hmax liste_lien =(
	
	  (*Permet de modifier un graph en ajoutant les bouts sur un étage*)
		let rec etage_tab_hmax0 liste_lien graph_tab n=(
		  match graph_tab with 
		    |[]->liste_lien
		    |l-> let l0=poser n liste_lien graph_tab in 
		    etage_tab_hmax0 (fst(l0)) (enlever_tab l (snd(l0))) (n+1)
		)in
		
		etage_tab_hmax0 (crea liste_lien) (table liste_lien) 1
	)in

	(* permet d'ordonner la liste pour avoir les vertex les plus gros avant *)
	List.fast_sort (fun (a1,b1,n1)  (a2,b2,n2) ->  n1-n2) (etage_tab_hmax liste_lien) 
;;

(**Placement simple des noeuds horizontalements -> chaque noeud a un emplacement distinct*)
let placement_h0 liste_position =

	let rec placement_h00 liste_position layer n=
	match liste_position with
		|[]->[]
		|(nom,x,y)::t_liste_position -> if y=layer then (nom,n,y)::(placement_h00 t_liste_position y (n+1))
																 else (nom,1,y)::(placement_h00 t_liste_position y 2)
	in
	
	placement_h00 liste_position 1 1
;;

(** III- Regroupement des informations **)

(**III-a Traductuion vertex **)


let vertex_of_graph' liste_lien=	

	(*Création d'une liste avec les positions des vertices *)
	let graph0 = placement_h0 (layering liste_lien) in
	
	(*initialisation de la liste de vertex *)
	let graph1= List.map ( fun (nom,x,y)-> {name=nom; next_vertices = []; abscissa= x; ordinate= y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}}) graph0 in
	
	(*permet de trouver le vertex tq vertex.name = nom de la liste *)
	let vert list_of_vertex nom = List.find (fun v-> v.name=nom) list_of_vertex in 
	
	(*permet d'ajouter les fils de chaque vertex récursivement *)
	let rec vertex_of_graph_bis' graph_tab list_of_vertex =
		match graph_tab with 
			|[]->()
			|(nom,liste)::t ->  ( 
				List.hd (List.map (fun v -> if v.name<>nom then () 
					else (v.next_vertices <- v.next_vertices@( List.map (vert list_of_vertex) liste) )) list_of_vertex); 
				vertex_of_graph_bis' t list_of_vertex 
			)
	in
	if liste_lien=[] then []
	else(
	  let ordonner_tb graph_tab= List.fast_sort (fun (nom1,l1) (nom2,l2) -> (List.length l1)- (List.length l2) ) graph_tab in
	  vertex_of_graph_bis' (ordonner_tb (table liste_lien)) graph1;
	  graph1
	)
;;

(** III-b Réduction des chemins    **)

(**Permet de placer le noeud vertex en (x,y) dans le graphe graph_vertex*)
let rec move vertex x y p_list_of_vertex=
p_list_of_vertex :=
 List.map 
 (fun v-> ( (if (v.name=vertex.name && v.abscissa=vertex.abscissa && v.ordinate =vertex.ordinate ) then (v.abscissa <- x;v.ordinate <-y)) ;v)) !p_list_of_vertex
;;

(**Permet de recuperer la largeur de chaque étage d'un vertex*)
	let info liste_of_vertex=(
		let rec info0 liste_of_vertex n=(
	  	match List.partition (fun vertex->vertex.ordinate= n ) liste_of_vertex  with
	  		|([],[])-> []
				|(liste_of_vertex1,liste_of_vertex2)-> (List.length liste_of_vertex1)::(info0 liste_of_vertex2 (n+1))
		)in
	(info0 liste_of_vertex 1)
	);;

(**Permet de faire +1 sur le n eme element d'une liste d'entier pointé*)
			let rec upp l n=( 
				match !l with 
					|[]->()
					|h::t ->if n<=0 then l:=(h+1)::t
				else (let pt=ref t in upp pt (n-1) ; l:=h::(!pt))
			);;
			
(**fonction  qui ordonne les graphes des vertices*)
let ordonner p_list_of_vertex = 
(*fonction interne qui ordonne les fils des vertices*)
	let rec ordonner_vertex vertex =(
		vertex.next_vertices <- 
		(List.fast_sort 
		(fun v1 v2 -> v1.abscissa - v2.abscissa) 
		(List.map (fun v->(ordonner_vertex v; v)) vertex.next_vertices) ) 
	) 
	in
	
	let ordonner' list_of_vertex = 
	(List.fast_sort 
	(fun v1 v2 -> if (v1.ordinate = v2.ordinate) then v1.abscissa -v2.abscissa 
					 else v1.ordinate - v2.ordinate) 
	(List.map (fun v-> (ordonner_vertex v;v)) list_of_vertex)
	) 
	in
	
	p_list_of_vertex :=( ordonner' !p_list_of_vertex)
;;

(**fonction qui permet de réduire des chemains trop long : tri vertical
Pemet de remonter les bout en dessous du noeud pere
Permet de faire remonter des chemains sans boucle
*)
let reduce_path p_list_of_vertex =

  (*fonction qui vérifie si le vertex mène vers un chemin sans boucle ou séparation*)
  let rec lone_path vertex vertex_before list_of_vertex =(
    match (List.filter (fun v-> (v.ordinate=vertex.ordinate+1)&&(List.mem vertex v.next_vertices)) list_of_vertex) with 
      |[] -> (match vertex.next_vertices with
                |[vertex0]-> (if vertex0= vertex_before then true else false)
                |_-> false
             )
      |[vertex3]-> lone_path vertex3 vertex list_of_vertex
      |[vertex4;vertex5] -> if vertex4=vertex_before then lone_path vertex5 vertex list_of_vertex
                       else if vertex5=vertex_before then lone_path vertex4 vertex list_of_vertex
                       else false
      |_->false
  )in
  (*fonction qui calcul le noeud pere le plus proche 
  filtre les chemins sans boucle/séparation pour les remonter*)
  let n_up vertex p_list_of_vertex=(
    let resultat =List.fast_sort (fun v1 v2 -> v1.ordinate-v2.ordinate) 
    (List.filter (fun v -> not(lone_path v vertex (!p_list_of_vertex)) ) 
    (List.filter (fun v -> List.mem vertex v.next_vertices) !p_list_of_vertex)) in
    if resultat= [] then (vertex.ordinate+1) else   (List.hd resultat).ordinate
  )in

  (*permet de remonter les noeuds à la nouvelle positions calculée*)
  if !p_list_of_vertex=[] then () 
  else (

    ordonner p_list_of_vertex;
    p_list_of_vertex:=List.rev !p_list_of_vertex;
    
    List.hd (List.map (fun v-> (
        let nb=n_up v p_list_of_vertex in
        if nb = v.ordinate+1 then ()
        else (move v (1) (nb-1) p_list_of_vertex) 
    ) ) !p_list_of_vertex);
  

  
  (*pemet de verifier s'il n'y a pas de noeuds_pere sous un de ses noeuds fils *)
  ordonner p_list_of_vertex;
    List.hd (
    List.map (fun v-> (
                      let h_max_fils = list_max (List.map (fun v->v.ordinate) v.next_vertices) in
                      if v.ordinate <= h_max_fils then  move v v.abscissa (h_max_fils+1) p_list_of_vertex
                      else ()
                      )) !p_list_of_vertex  
    );
  );
    (*pemet de verifier que chaque noeud a une position distincte *)
    let verifier list_of_vertex p_list_of_vertex=(
    
      let rec verifier' list_of_vertex p_list_of_vertex=(
       match list_of_vertex with 
         |(v0::(v1::t)) -> (if v0.ordinate =  v1.ordinate && (v0.abscissa+1) <> v1.abscissa then move v1 (v0.abscissa+1) v1.ordinate p_list_of_vertex
                       else if v0.ordinate <> v1.ordinate                                   then move v1         1       v1.ordinate p_list_of_vertex;
                            v0::(verifier' (v1::t) p_list_of_vertex)
                            )
         |l->l (*cas []->[] et [v]->[v]*)
      )in
      
      match list_of_vertex with 
          |(v0::(v1::t)) -> (move v0 (1) v0.ordinate p_list_of_vertex;
                           verifier'  (v0::(v1::t)) p_list_of_vertex)
                          
          |l->l (*cas []->[] et [v]->[v]*)
    )in
    ordonner p_list_of_vertex;
    p_list_of_vertex:=verifier !p_list_of_vertex p_list_of_vertex;
    ordonner p_list_of_vertex
;;

(**fonction qui place les noeuds hors de l'ecran (x->-x) pour mieux les replacer après
Permet d'avoir plusieurs empty vertex au même endroit 
*)
let rec change_layer00 liste p_list_of_vertex=
			match liste with
					|[]->()
					|(nom,x,y)::t -> ( move {name=nom; next_vertices=[] ; abscissa = x; ordinate = y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} (-x) y p_list_of_vertex ;
            change_layer00 t p_list_of_vertex)
;;

(** III-c Ajouter les noeuds virtuels dans un graphe vertex **)

(*Ajoute les noeuds virtuels dans la liste de vertex*)
let add_dummy p_list_of_vertex =

	(*Poiteur vers la liste des largeurs de chaques étage () construit a partir du vertex le plus grand*)
	let p_info = ref (info !p_list_of_vertex ) in
			
	let rec add_dummy_h p_info vertex =(
		
		(*fonction qui ajoute un élément à la fin d'un liste pointé*)
		let rec add_end element p_liste = p_liste := (!p_liste)@[element] in
					
		(*fonction qui informe s'il faut ajouter un noeud virtuel apres un vertex de la liste avec sa position*)
		let rec absc vertex_list nb= (

			let n = try List.nth (!p_info) (nb) with |Failure("nth") -> 0 in

			match vertex_list with
				|[]->[]
				|h::t -> if (nb+2-h.ordinate <> 1) then ( (upp p_info nb); (h,n+1)::(absc t nb) )
								 else (h,0)::(absc t nb)
		)in
		
		if vertex.next_vertices =[] then ()
				else (
				let liste_largeur =  ( absc vertex.next_vertices (vertex.ordinate-2) )in
				vertex.next_vertices <- List.map (fun (vertex_fils,n)-> if vertex.ordinate-vertex_fils.ordinate = 1 then  (add_dummy_h p_info vertex_fils;vertex_fils)
												else (let empt={name="empty_vertex";next_vertices = [vertex_fils];abscissa = n; ordinate = (vertex.ordinate-1); draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} in
												 add_end empt p_list_of_vertex ; add_dummy_h p_info empt ;empt)
												) liste_largeur )
	
	)in
  if !p_list_of_vertex=[]then ()
  else List.hd (	List.map (add_dummy_h p_info) !p_list_of_vertex)
;; 



(** III-d Crossing reduce **)

(**Permet d'obtenir les lien entre deux étages
[(1,2);(3,2);(3,1)] -> [(1,[2]);(3,[1;2])] *)
let layer list_of_vertex n=
	let layer' = List.filter (fun vertex->vertex.ordinate= (n+2) ) list_of_vertex  in
	let fils   = no_occ (List.concat (List.map (fun v-> v.next_vertices) layer'))  in
	let pos vertex = (vertex.name,vertex.abscissa,vertex.ordinate) in

	let etage0 = List.map    (fun vertex -> (pos vertex , List.map pos vertex.next_vertices))       layer'         in
	(*on doit ajouter les pere sans fils*)
	let etage1 = List.filter (fun vertex -> (vertex.ordinate= n+1 && not( List.mem vertex fils ) )) list_of_vertex in 
	let etage1 = List.map    (fun vertex -> (pos vertex , [pos vertex]))                            etage1         in
	(*On distribue les noeuds son fils sur les bords*)
	let rec distribuer l1 l2=
	  match l1 with
	    |[ ]  -> l2 
	    |[h0] -> h0::l2
	    |h1::(h2::t)-> distribuer t ((h1::l2)@[h2])
	in
	distribuer etage0 etage1
;;

(**fonction qui donne une liste de liens d'un étage : père des fils
[(1,2);(3,2);(3,1)] -> [(2,[1;3]);(1,[3])]*)
let layer' list_of_vertex n=
	
	let rec completer' liste1 liste2 element =
		match liste1 with 
			|[]-> liste2
			|h::t -> completer' t ( (element,h)::liste2 ) element
	in
	
	let etage = layer list_of_vertex n in
	let etage_lien = List.concat  (List.map ( fun (pere,list_fils) -> completer' list_fils [] pere) etage) in
	let liste_fils = snd (List.split etage_lien) in
	
	let rec layer'' liste_lien element liste =
		match liste_lien with
			|[]   -> liste 
			|(pere,fils)::t -> if fils=element then  layer'' t element [pere]@liste
													else layer'' t element liste
	in
	no_occ (List.map (fun element -> (element, no_occ (layer'' etage_lien element []) ) ) liste_fils)
;;
	
(**fonction qui reduit les croisement grossièrement : logique fils->pere*)
let less_crossing p_list_of_vertex =
  ordonner p_list_of_vertex;
	let h_max =(list_max (List.map (fun v->v.ordinate) !p_list_of_vertex))-1 in 
	
	let rec less_crossing' n p_list_of_vertex=
	
		let p_compteur = ref 0 in

		(*fonction qui place ordonne grrosièrement un étage *)
		let rec change_layer liste p_list_of_vertex =(
				match liste with
					|[]->()
					|(nom,x,y)::t -> (  
							p_compteur:=(!p_compteur+1);
							move {name=nom; next_vertices=[] ; abscissa = -x; ordinate = y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} !p_compteur y p_list_of_vertex ;
							change_layer t p_list_of_vertex
							);
		)in	
		ordonner p_list_of_vertex;
		let etage = no_occ (List.concat (snd (List.split ( (no_occ(layer !p_list_of_vertex n ))))))in
		
		if etage=[] then ()
		else (
			change_layer00 etage p_list_of_vertex; change_layer etage p_list_of_vertex; less_crossing' (n-1) p_list_of_vertex )
	in
	less_crossing' h_max p_list_of_vertex
;;

(**fonction qui reduit les croisement grossièrement : logique pere->fils*)
let less_crossing' p_list_of_vertex=
	
	let rec less_crossing'' n p_list_of_vertex=
	
		let p_compteur = ref 0 in
				
		(*fonction qui place ordonne grrosièrement un étage *)
		let rec change_layer liste p_list_of_vertex =(
				match liste with
					|[]->()
					|(nom,x,y)::t -> (  
							(p_compteur:=(!p_compteur+1);
							move {name=nom; next_vertices=[] ; abscissa = -x; ordinate = y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} !p_compteur y p_list_of_vertex );
							change_layer t p_list_of_vertex
							);
		)in	
		let liste_fils_ordonnee = List.fast_sort (fun ((a1,x1,y1),l1) ((a2,x2,y2),l2)->if x1=x2 then List.length l1 - List.length l2 else x1-x2  ) (no_occ(layer' !p_list_of_vertex n )) in
		let etage = no_occ (List.concat (snd (List.split  liste_fils_ordonnee)))in
		
		if etage=[] then ()
		else (change_layer00 etage p_list_of_vertex ; change_layer etage p_list_of_vertex; less_crossing'' (n+1) p_list_of_vertex )
	in
	less_crossing'' 0 p_list_of_vertex
;;

(**IV Finalisation**)

(**IV- a Centrer les graphes **)

(**fonction qui reduit les croissement et centre les noeuds pour un étage
avec etage du bas fixé comme référence pour choisir les barycentres*)
let center_fixed_son p_list_of_vertex y =
  let n=y-2 in
  (*on ne tient pas en compte les bout sans père *)
  let etage = List.filter (fun (vertex,liste) -> liste <> [vertex] ) (layer !p_list_of_vertex n) in 
  let etage = if etage = [] then layer !p_list_of_vertex n else etage in
  let bar liste=
    let rec bar' liste nb=
      match liste with
        |[]-> nb
        |(_,x,_)::t_vertices -> bar' t_vertices (nb+x)
    in
    let n = match List.length liste with |0->1 |nb->nb in
    let r0 = (bar' liste 0)/ n in
    let r = (float_of_int (bar' liste 0) )/. (float_of_int n) in
    
    if r-.float_of_int(r0) >0.5 then r0+1
                               else r0
  in
  
  let rec barycenter liste =
    let rec barycenter' list_in list_out x0 n0 =
      match list_in with
        |[]                -> list_out
        |((name,x,y),n)::t -> if n=x0 then (if n0>=0 then barycenter' t (((name,x,y),n+n0,n)::list_out) x0 (-n0 - 1)
                                                         else barycenter' t (((name,x,y),n+n0,n)::list_out) x0 (-n0    ) )
                                          else                barycenter' t (((name,x,y),n   ,n)::list_out) n  (     -1)
    in
    barycenter' liste [] 0 0
  in 
  
  

  let verifier liste0 =
    let rec verifier' liste0 liste1 =
      match liste0 with
        |[]                    ->    liste1
        |[h]                   -> h::liste1
        |((name0,x0,y0),n0)::(((name1, x1,y1),n1) ::t)  -> if n0 < n1 then verifier' ( ((name1, x1,y1), n1   )::t )  (((name0, x0,y0),n0)::liste1)
                                                                      else verifier' ( ((name1, x1,y1),(n0+1))::t )  (((name0, x0,y0),n0)::liste1)
    in
    match liste0 with
      |[]                    ->    []
      |((name,x,y),n)::t -> if n <=0 then verifier' (((name, x,y),1)::t) []
                                     else verifier' (((name, x,y),n)::t) []
  in
  let etage0 = List.map (fun (tripl,liste_triplet)->tripl) (List.filter (fun (triplet,liste) -> liste=[]) (layer !p_list_of_vertex (n-1))) in
  let etage = List.map (fun ((vertex_name, x,y),liste)->
        ( (vertex_name, x,y),List.filter (fun triplet-> (not(List.mem triplet etage0))) liste  )  )
        etage
  in
   if etage=[]then ()
  else (
      let etage = List.fast_sort 
      ( fun ((name0, x0,y0),n0,liste0) ((name1, x1,y1),n1,liste1)-> 
      if n0<>n1 then n0-n1
                else (List.length liste0) -(List.length liste1)
      ) 
          (List.map (fun ((vertex_name, x,y),liste)->
          (let b=bar liste in
          if b=0 then ((vertex_name, x,y),x         ,liste)
                 else ((vertex_name, x,y),bar liste ,liste)  )) etage)
      in

      (*Après le tri on enlève l'infomation sur les fils*)
      let etage = List.map ( fun ((name0, x0,y0),n0,liste0)->((name0, x0,y0),n0)) etage in
        
        let etage = List.fast_sort ( fun ((name0, x0,y0),n0,b0) ((name1, x1,y1),n1,b1)->if n0=n1 then b0-b1 else n0-n1) (barycenter  etage) in
        let etage = List.map ( fun ((name, x,y),n,b) -> ((name, x,y),n)) etage in

      let etage = verifier (List.fast_sort ( fun ((name0, x0,y0),n0) ((name1, x1,y1),n1)-> n0-n1) etage) in

      List.hd (List.map (fun ((vertex_name, x,y),_    )-> move {name=vertex_name;next_vertices=[];abscissa=  x ;ordinate=y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} (-x)        y p_list_of_vertex) etage);
      List.hd (List.map (fun ((vertex_name, x,y),n)-> move {name=vertex_name;next_vertices=[];abscissa=(-x);ordinate=y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} n y p_list_of_vertex) etage)
      )
;;

(**fonction qui reduit les croissement et centre les noeuds pour un étage
avec etage du haut fixé comme référence pour choisir les barycentres*)
let center_fixed_father p_list_of_vertex y=
  let n=y-1 in
  let etage = layer' !p_list_of_vertex n in
  let etage = List.fast_sort 
  (fun (vertex1,liste1) (vertex2,liste2) -> List.length liste2 - List.length liste1) etage 
  in
  
  (*Permet de calculer les barycentre d'une liste de vertices*)
  let bar liste=
    let rec bar' liste nb=
      match liste with
        |[]-> nb
        |(_,x,_)::t_vertices -> bar' t_vertices (nb+x)
    in
    let n = match List.length liste with |0->1 |nb->nb in
    let r0= (bar' liste 0)/ n in
    let r = (float_of_int (bar' liste 0) )/. (float_of_int n) in
    
    if r-.float_of_int(r0) >0.5 then r0+1
                               else r0
  in
  
  (*Permet de distribuer les noeuds autours du barycentre calculé
  on garde l'ancien baricentre pour un tri plus efficace*)
 let rec barycenter liste =
    let rec barycenter' list_in list_out x0 n0 =
      match list_in with
        |[]                -> list_out
        |((name,x,y),n)::t -> if n=x0 then (if n0>=0 then barycenter' t (((name,x,y),n+n0,n)::list_out) x0 (-n0 - 1)
                                                     else barycenter' t (((name,x,y),n+n0,n)::list_out) x0 (-n0    ) ) 
                                      else                barycenter' t (((name,x,y),n   ,n)::list_out) n  (     -1)
    in
    barycenter' liste [] 0 0
  in  

  
  (*Permet de réordonner les noeuds en cas de superposition*)
  let verifier liste0 =
    let rec verifier' liste0 liste1 =
      match liste0 with
        |[]                    ->    liste1
        |[h]                   -> h::liste1
        |((name0,x0,y0),n0)::(((name1, x1,y1),n1) ::t)  -> if n0 < n1 then verifier' ( ((name1, x1,y1), n1   )::t )  (((name0, x0,y0),n0)::liste1)
                                                                      else verifier' ( ((name1, x1,y1),(n0+1))::t )  (((name0, x0,y0),n0)::liste1)
    in
    match liste0 with
      |[]                    ->    []
      |((name,x,y),n)::t -> if n <=0 then verifier' (((name, x,y),1)::t) []
                                     else verifier' (((name, x,y),n)::t) []
  in
  let etage0 = List.map (fun (tripl,liste_triplet)->tripl) (List.filter (fun (triplet,liste) -> liste=[]) (layer !p_list_of_vertex (n-1))) in
  let etage = List.map (fun ((vertex_name, x,y),liste)->
        ( (vertex_name, x,y),List.filter (fun triplet-> (not(List.mem triplet etage0))) liste  )  )
        etage
  in
  if etage=[]then ()
  else (
      (* On ajouter comme information sur chaque noeuds ses fils et le barycentre de ses fils 
      Puis on ordonne la liste selon les barycentres si égalité on compare le nombre de fils*)
      let etage = List.fast_sort 
      ( fun ((name0, x0,y0),n0,liste0) ((name1, x1,y1),n1,liste1)-> 
      if n0<>n1 then n0-n1
                else (List.length liste0) -(List.length liste1)
      ) 
          (List.map (fun ((vertex_name, x,y),liste)->
          (let b=bar liste in
          if b=0 then ((vertex_name, x,y),x         ,liste)
                 else ((vertex_name, x,y),bar liste ,liste)  )) etage)
      in
  
      (*Après le tri on enlève l'infomation sur les fils*)
      let etage = List.map ( fun ((name0, x0,y0),n0,liste0)->((name0, x0,y0),n0)) etage in
  
  
      (*Utilisation de BARYCENTER 
      puis trie de la liste des barycentre pour les réordonner *)
      let etage = List.fast_sort ( fun ((name0, x0,y0),n0,b0) ((name1, x1,y1),n1,b1)->if n0=n1 then b0-b1 else n0-n1) (barycenter etage) in
      let etage = List.map ( fun ((name, x,y),n,b) -> ((name, x,y),n)) etage in
  
      (*Utilisation de véridfier*)
      let etage = verifier (List.fast_sort ( fun ((name0, x0,y0),n0) ((name1, x1,y1),n1)-> n0-n1) etage) in

      List.hd (List.map (fun ((vertex_name, x,y),_)-> move {name=vertex_name;next_vertices=[];abscissa=  x ;ordinate=y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}} (-x) y p_list_of_vertex) etage);
      List.hd (List.map (fun ((vertex_name, x,y),n)-> move {name=vertex_name;next_vertices=[];abscissa=(-x);ordinate=y; draw = {vertex_shape = Rectangle; vertex_color = Graphicspdf.yellow; edge_width = 1; edge_color = Graphicspdf.green}}   n  y p_list_of_vertex) etage)
      )
;;


(**fonction qui reduit les croissement et centre les noeuds pour le graphe entier*)
let center p_list_of_vertex =

  (*on commence par chercher l'etage avec le plus de noeuds*)
  let width_max list_of_vertex=
    let rec width_max' list_of_vertex y width layer= 
      match List.partition (fun v-> v.ordinate=y) list_of_vertex with
        |(_,[])->layer
        |(l0,l1)-> (if List.length l0> width then width_max' l1 (y+1) (List.length l0) y
                   else width_max' l1 (y+1) width layer)
    in
   width_max' list_of_vertex 1 0 0
  in
  
  let h_max =(list_max (List.map (fun v->v.ordinate) !p_list_of_vertex))-1 in 
  let layer_w = width_max !p_list_of_vertex in
  
  let rec center_up p_list_of_vertex n = 
    if n > (h_max+1) then ()
    else (if n = (h_max+1) then center_fixed_son p_list_of_vertex n
    else (center_fixed_son p_list_of_vertex n;        (*etage n-1 fixe ->  n  modifié*)
          center_fixed_father p_list_of_vertex (n-1); (*etage  n  fixe -> n-1 modifié*)
          center_fixed_son p_list_of_vertex n;        (*etage n-1  fixe ->  n  modifié*)
          center_fixed_son p_list_of_vertex (n-1);    (*etage n-2 fixe -> n-1 modifié*)
          center_up p_list_of_vertex (n+1) )
          )
  in
  
  
  let rec center_down p_list_of_vertex n=
    if n < 1 then ()
    else (if n = 1 then center_fixed_father p_list_of_vertex n
    else (center_fixed_father p_list_of_vertex n;      (*etage n+1 fixe ->  n  modifié*)
          center_fixed_son p_list_of_vertex (n+1);     (*etage  n  fixe -> n+1 modifié*)
          center_fixed_father p_list_of_vertex n;      (*etage n+1 fixe ->  n modifié*)
          center_fixed_father p_list_of_vertex (n+1);  (*etage n+2 fixe -> n+1 modifié*)
          center_down p_list_of_vertex (n-1) )
          )
  in
  
  let rec center_up' p_list_of_vertex n = 
    if n > (h_max+1) then ()
    else (
          if n = (h_max+1) then center_fixed_son p_list_of_vertex n
          else (center_fixed_son p_list_of_vertex n;
                center_up' p_list_of_vertex (n+1) )
         )
  in
  let rec center_down' p_list_of_vertex n=
    if n < 1 then ()
    else (
          if n = 1 then center_fixed_father p_list_of_vertex n
          else (center_fixed_father p_list_of_vertex n;
                center_down' p_list_of_vertex (n-1) )
        )
  in
  let n0 = Random.int 3 in
  if  n0 = 0 then (
        center_fixed_father p_list_of_vertex (layer_w-1);
        center_fixed_son    p_list_of_vertex (layer_w  );
        )
  else if n0=1 then(
        center_fixed_son    p_list_of_vertex (layer_w+1);
        center_fixed_father p_list_of_vertex (layer_w);
        );
  if layer_w < h_max then center_up   p_list_of_vertex (layer_w+1);
  if layer_w > 0     then center_down p_list_of_vertex (layer_w-1)
  else ();
  if layer_w < h_max then center_up'   p_list_of_vertex (layer_w+1);
  if layer_w > 0     then center_down' p_list_of_vertex (layer_w-1)
  else (); 
;;


(**supprimer les empty vertex inutiles*)

let rec remove_empty_vertex p_list_of_vertex =
  
  let rec end_linear_path p_list_of_vertex =
    
    (*cherche le bout du chemin droit en empty_vertex*)
    let rec end_linear_path' vertex=(
      let resultat = List.partition (fun v->( (v.abscissa =vertex.abscissa) && v.name="empty_vertex")) vertex.next_vertices in
      match fst resultat with
        |[]   -> vertex
        |h::t -> end_linear_path' h
    )in
    match !p_list_of_vertex with
      |[]->[]
      |h_vertex::t_vertices ->
          (
            (*modifie les fils du vertex*)
            let resultat = List.partition (fun v->( (v.abscissa =h_vertex.abscissa) && v.name="empty_vertex")) h_vertex.next_vertices in
            
            match fst resultat with
              |[]   -> end_linear_path (ref t_vertices)
              |vertex0::t ->(
                  if end_linear_path' vertex0 = vertex0 then end_linear_path (ref t_vertices)
                  else
                  (h_vertex.next_vertices <- (end_linear_path' vertex0)::(List.filter (fun v->v<>vertex0) h_vertex.next_vertices);
                  vertex0::(end_linear_path (ref t_vertices)))
                )
          )
  in
  (*on enleve les noeuds supprimés de la liste de vertex*)
  ordonner p_list_of_vertex;
  let liste_remove = end_linear_path p_list_of_vertex in
  ();
  p_list_of_vertex:=List.filter (fun v-> not(List.mem v liste_remove)) !p_list_of_vertex;

;;


(**IV- b Re-inserer les cycles **)

(**fonction qui permet de remettre les flèches dans le bon sens*)
let rec change_arrow list_of_arrow p_list_of_vertex=

  (* Permet d'echanger les entre chaque empty_vertex entre name1 et name 2 *)  
  let change_arrow' (name1,name2) p_list_of_vertex =(
    
    (*fonction test si on est dans le chemin de empty_vertex vers name *)
    let rec test_empty_remove' name vertex = (
      if vertex.name="empty_vertex" then test_empty_remove' name (List.hd vertex.next_vertices)(*les empty vertex n'ont que 1 noeud fils*)
      else (if vertex.name = name then true
                                  else false  )
    )in  
  
    (* Permet d'echanger la flèche entre deux vertex père/fils *)  
    let switch_f_s vertex_f vertex_s p_list_of_vertex =(
      (*On enlève le vertex fils comme fils du vertex père pour le graphe*)
      let rec switch_no_son list_of_vertex=
        match list_of_vertex with
          |[]->()
          |vertex::t-> if (vertex == vertex_f) then 
                                  vertex.next_vertices <- ( List.filter (fun v->  not( v == vertex_s)) vertex.next_vertices)
                       else switch_no_son t
      in
      switch_no_son !p_list_of_vertex;
      
      (*On modifie le vertex père : on enlève le vertex fils comme noeud fils*)
      vertex_f.next_vertices <-  ( List.filter (fun v->   not( v == vertex_s)) vertex_f.next_vertices);
      
      (*On ajoute le vertex pere comme fils du vertex fils pour le graphe*)
      let rec switch_no_father list_of_vertex=
        match list_of_vertex with
          |[]->()
          |vertex::t->if (vertex == vertex_s) then 
                      vertex.next_vertices <- (vertex_f)::vertex.next_vertices
                             else (switch_no_father t)
      in
      switch_no_father !p_list_of_vertex;
    )in

    let rec change_arrow'' vertex1 vertex2 p_list_of_vertex =
      switch_f_s vertex1 vertex2 p_list_of_vertex;
      if vertex2.name=name2 then ()
      else (
      let vertex3 = List.find (test_empty_remove' name2) vertex2.next_vertices in
      change_arrow'' vertex2 vertex3 p_list_of_vertex)
    in

    (*Recherche du noeud de départ pour la flèche et le noeud suivant pour initialiser l'inversion*)
    let vertex_top = List.find (fun vertex-> vertex.name = name1) !p_list_of_vertex in
    let vertex_son = List.find (test_empty_remove' name2) vertex_top.next_vertices in
    change_arrow'' vertex_top vertex_son p_list_of_vertex
  )in

  match list_of_arrow with
    |[]->()
    |h_arrow::t_arrow -> (change_arrow' h_arrow p_list_of_vertex ;
                          change_arrow t_arrow p_list_of_vertex)
;;


(**Permet d'ajouter les lien donnés par une liste de fleche*)
let rec add_link_list liste list_of_vertex=
  
  let add_link (a,b) list_of_vertex=
  
    let rec add_link' nom vertex list_of_vertex=
    match list_of_vertex with
      |[]->()
      |h_vertex::t_vertices -> if (h_vertex.name=nom) then h_vertex.next_vertices <-vertex::h_vertex.next_vertices
    in
    
    let vertexb=List.find (fun v-> v.name=b) list_of_vertex in
    add_link' a vertexb list_of_vertex;
  in

  match liste with
    |[]->list_of_vertex
    |h::t-> (add_link h list_of_vertex;add_link_list t list_of_vertex)
;;
(**fonction qui convertie une liste de lien en liste de vertices 
   marche pour graphe cyclique*)


let vertex_of_graph liste_lien =

   let vertex_of_graph'' liste_lien =
    if liste_lien=[] then []
    else
      (
      let liste_lien'= cycle_removal liste_lien in
      let graph = ref (vertex_of_graph' (fst liste_lien')) in
      change_arrow (snd liste_lien') graph;
      !graph
      )
  in
  
  let list1=List.partition (fun (a,b)->a<>b) liste_lien in 
  let list2=snd list1 in
  let list1=fst list1 in
  
  let list1 =if list1 <> [] then list1 else (List.map (fun (a,b)->(a,"empty_vertex")) list2)@(List.map (fun (a,b)->("empty_vertex",a)) list2)in
      let list_of_vertex0 = vertex_of_graph'' list1 in

      let rec add_son list_of_vertex nom =
        let vertex= List.find (fun v->v.name=nom) list_of_vertex in
        vertex.next_vertices <- vertex.next_vertices@[vertex]
      in

      if list2=[]then()
      else (List.hd (List.map (fun(nom,nom') -> add_son list_of_vertex0 nom) list2));
      list_of_vertex0
;;
(**IV- c: Fonction de trie / creation finale **)

let graph_layout graphe=
  
  if List.length graphe <= 1 then (vertex_of_graph graphe)
  else (
   
    (*On recupère les noeuds qui pointent sur eux même*)
    let graph  = List.partition (fun (a,b)-> a<>b) graphe in
    let graph0 = snd graph in
    let graph  = fst graph in
  
    (*Supprimer les cycles et garder les flèches modifiées en mémoire*)
    let graph_a=cycle_removal graph in 
    (* liste des flèches modifiées*)
    let graph_a_fleche =snd  graph_a in
    (* Creation du graph acyclique *)
    let graph_a=fst graph_a in
    (* Liste des flèches à rajouter:
       lors de l'inverion des flèches certines sont supprimées *)
    let graph_b=List.filter (fun x-> List.mem x graphe) graph_a_fleche in
  
    (* traduction en liste de vertex *)
    let graph_vv=vertex_of_graph graph_a in 
    let graph_w=ref graph_vv in

    (* Reduction des empty_vertex en remontant certains noeuds*)
    reduce_path graph_w ; 
   
    (* Ajout des noeuds virtuels*)
    add_dummy graph_w;

    (* Reduction des croisements (grossier) *)
    less_crossing' graph_w;
    less_crossing graph_w;

    (*Reduction des croisements + centrer le graphe *)
    center graph_w;
  
    (*Permet de supprimer les empty_vertex aligné*)
    remove_empty_vertex graph_w;

    (*Permet de remettre les cycles *)
    change_arrow graph_a_fleche graph_w;
    
    (*Permet de remettre les flèches supprimées*)
    graph_w := if graph_b<>[] then add_link_list graph_b !graph_w
    else !graph_w;
    
    if graph0<>[] then add_link_list graph0 !graph_w
    else !graph_w;
    )
;;
