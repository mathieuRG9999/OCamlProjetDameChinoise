let dim = 3

type case = int * int * int

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre 
  | Dehors


let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"

type case_coloree = case * couleur;;
type configuration = case_coloree list * couleur list;;
type coup = Du of case * case | Sm of case list;;


(*pour les cases i<dim, il s'agit du triangle ou devront être les cases vertes au début*)
(*pour les cases i>dim, il s'agit du triangle ou devront être les cases noirs au début*)

let to_string_case (c:case) = 
  let (x,y,z) = c in
  "(" ^ string_of_int(x)^" ,"^string_of_int(y)^" ,"^string_of_int(z) ^ ")";;

let est_dans_losange (c: case) (dim:int) =
  let (x,y,z) = c in
  if (x+y+z=0) &&
    ( x <= 2*dim && x >= (-2)*dim) &&
    (y <= dim && y >= -dim) &&
    (z <= dim && z >= -dim)
     then true
  else false;;



  let est_dans_etoile (c:case) =
    let (i, j, k)=c in 
    if  (est_dans_losange (i, j, k) dim || est_dans_losange (j, k, i) dim || est_dans_losange (k,i,j) dim)   then true
    else false;;



    let rec tourne_case m (x:case)=
      let (a, b, c)=x in 
      if m mod 6 =0 then x
      else  tourne_case (m-1) (-c,-a,-b);;



    let translate c v = 
    let (c1,c2,c3)= c in( let (v1,v2,v3)=v in (c1+v1, c2+v2, c3+v3));;
(*
    let test_translate = 
      print_string ("la translation du point (-4,1,3) avec le vecteur (0,2,-2) donne le point " ^ to_string_case(translate (-4,1,3) (0,2,-2))); (*(-4,3,1)*)
      print_string ("la translation du point (-4,3,1) avec le vecteur (1,0,-1) donne le point " ^ to_string_case(translate (-4,3,1) (1,0,-1)));(*(-3,3,0)*)
      print_string ("la translation du point (-4,3,1) avec le vecteur (7,0,-486) donne le point " ^ to_string_case(translate (-4,3,1) (7,0,-486)));;(*(-3,3,-485)*)
*)
    let diff_case c1 c2= 
    let (x, y, z)=c1 in (let (x1, y1, z1)=c2 in ((x-x1, y-y1, z-z1)));;
(*
    let test_diff_case = 
      print_string ("la différence entre les points (0,0,0) et (0,-2,2) est " ^ to_string_case(diff_case (0,0,0) (0,-2,2))); (*(0,-2,-2)*)
      print_string ("la différence entre les points (7,0,0) et (0,-2,2) est " ^ to_string_case(diff_case (7,0,0) (0,-2,2)));;(*(7,-2,-2)*)
*)

      (*Pareil ici on peut rentrer des cases interdites*)


      (*Ici on fait bcp trop de cas car case voisine peut ne pas exister*)
      (*Voir peut être juste garder les cas où on fait +1 et -1*)
      (*Changer ici*)
    let  sont_cases_voisines case1 case2= 
    let (x, y, z)=case1 in 
    let (x1,y1,z1)=case2 in 
      if 
        (x,y,z)=(x1, y1+1, z1-1) ||
        (x,y,z)=(x1, y1-1, z1+1) ||
        (x,y,z)=(x1+1,y1-1,z1) ||
        (x,y,z)=(x1-1,y1+1,z1) ||
        (x,y,z)=(x1+1,y1,z1-1) ||
        (x,y,z)=(x1-1,y1,z1+1) 
       then true
    
    else false;;


    (*let test_sont_case_voisines = 
      print_string(string_of_bool (sont_cases_voisines (-2,1,1) (-2,2,2)));
      print_string(string_of_bool (sont_cases_voisines (0,0,0) (0,1,1)));*)


    let alignement c1 c2 = (*je ne sais pas encore si cette fonction marchent et si elle est nécessaire*)
      let (x,y,z)=c1 in 
      let (x1,y1,z1)= c2 in 
        if  x= x1||y=y1||z=z1 then true
        else false;;


      (*Problème avec calcul pivot voir fonction test*)


    let calcul_pivot case1 case2 = (*on vérifie si les cases sont bien alignés*)(*on pourrait aussi vérifier que les deux éléménts sont dans bien dans le plateau*)
    if case1=case2 then None
    else if alignement case1 case2 then  (*on est aligné*)
      let (x, y,z) =case1 in
        let (x1,y1, z1)=case2 in 
          let xfinal=(x+x1) in 
            let yfinal=(y+y1) in
              let zfinal=(z+z1) in 
                if (xfinal mod 2=0)&&(yfinal mod 2=0)&&(zfinal mod 2 =0) then Some (xfinal/2, yfinal/2, zfinal/2)
                else None
    else None;;


(*
    calcul_pivot (0,-6,6) (0,-6,6);;
*)
(*

    let test_calcul_pivot =
      print_string "Calcul du pivot pour (0, -2, 2) et (0, 2, -2) : ";
      match calcul_pivot (0, -2, 2) (0, 2, -2) with
        | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
        | None -> print_string "Pas de pivot";

      print_string "Calcul du pivot pour (0, 0, 0) et (-4, 2, 2) : ";
        match calcul_pivot (0, 0, 0) (-4, 2, 2) with
          | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
          | None -> print_string "Pas de pivot";

      print_string "Calcul du pivot pour (0, 0, 0) et (2, 0, -2) : ";
        match calcul_pivot (0, 0, 0) (2, 0, -2) with
            | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
            | None -> print_string "Pas de pivot" (*Devrait fonctionner*)
 
*)
    let vec_et_dist case1 case2 =
      let vecteur=diff_case case1 case2 in let (x, y,z)= vecteur in let nombre=(abs x +abs y + abs z)/2 
    in (((x/nombre, y/nombre, z/nombre)), nombre);;
    
(*
    let test_vec_et_dist =
      print_string "Vecteur et distance pour (0, -2, 2) et (0, 0, 0) : ";
      match vec_et_dist (0, -2, 2) (0, 0, 0) with
      | ((vx, vy, vz), distance) -> print_string( "Vecteur : (" ^ string_of_int vx ^ "," ^ string_of_int vy ^ "," ^string_of_int vz ^") et la Distance : " ^ string_of_int distance)
      | _ -> print_string "Erreur : Impossible de calculer le vecteur et la distance";;

      print_string "Vecteur et distance pour (-6, 3, 3) et (-4, 3, 1) : ";
      match vec_et_dist (-6, 3, 3) (-4, 3, 1) with
      | ((vx, vy, vz), distance) -> print_string( "Vecteur : (" ^ string_of_int vx ^ "," ^ string_of_int vy ^ "," ^string_of_int vz ^") et la Distance : " ^ string_of_int distance)
      | _ -> print_string "Erreur : Impossible de calculer le vecteur et la distance";;

*)


(*On passe maintenant à la partie 2*)

(* on est le 23/11/2024 et il est 22:38*)
(*question 11*)
let rec tourne_liste l=
      match l with 
      | [] -> [] 
      |[x] -> []
      | x::q -> q @ [x];;
(*Le test
      tourne_liste [Vert;Jaune;Rouge];;
*)

let rec der_liste l= (*je crois qu'il y a une erreur sur la dernière question de son exemple *)
      match l with 
      | [] -> []
      | [x] -> [x]
      | _::q -> der_liste q;;



(*Le test :

      der_liste [2;5;8];;

*)

(*question 12*)



let rec remplir_segment m (c:case) = (*fonctionne comme c'est demandé*)
    let (i,j, k)=c in
      match m with
      | 0 -> []
      | _ -> (i,j,k)::remplir_segment (m-1) (i,j+1,k-1);;


(*Le test:

(remplir_segment 1 (0, 0, 0));;

(remplir_segment 3 ((-4), 1, 3));;
*)

(*question 13*)
let rec remplir_triangle_bas m (c:case) =(*on force le type pour la simplicité de relecture, il nous faudrait une fonction pour inverser*)
      let (i, j, k)= c in 
    match m with 
    | 0 -> []
    | _ -> (remplir_segment m (i, j, k)) @ remplir_triangle_bas (m-1) (i-1, j+1, k);; (*est ce qu'on peut utiliser le "a" ?*)

(*Les petits testes 
    remplir_triangle_bas 1 (0,0,0) = [(0,0,0)];;

    remplir_triangle_bas 3 (-4,1,3);;
*)


(*on passe à la question 14*)

let rec remplir_triangle_haut m (c:case) =(*on force le type pour la simplicité de relecture, il nous faudrait une fonction pour inverser*)
      let (i, j, k)= c in 
    match m with 
    | 0 -> []
    | _ -> (remplir_segment m (i, j, k)) @ remplir_triangle_bas (m-1) (i+1,j, k-1);; 

(*!!!!!!

J ene sais pas de quoi il me parle la : "On donnera d’abord des équations récursives définissant cette fonction formalisant l’idée qu’un triangle est composé d’un segment du bas et du triangle
au-dessus de ce segment"

*)

(*Los testos 
    remplir_triangle_bas 1 (0,0,0);;
    remplir_triangle_bas 3 (-4,1,3);;
*)



(*on passe à la question 15*)


let rec colorie (coul:couleur) liste = (*test à faire pour cette fonction*)
  match liste with (*on doit ajouter à chaque élément de la listela couleur coul*)
  | [] -> []
  | t::q->(t, coul):: colorie coul q;;

(*je n'ai pas compris la q 16 et la 17*)
let rec tourne_config_aux (liste: case_coloree list) angle : case_coloree list= (*On doit tourner de N/36 de ce que j'ai compris*)
    match liste with 
     |[] -> []
      | (case, couleur)::q -> (tourne_case angle case, couleur)::tourne_config_aux q angle;;


let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ]);;

  let tourne_config (config:configuration) : configuration= (*A REVOIR*)
    let (case_list, couleur_list)=config in
    match couleur_list with 
    | [] -> (case_list, couleur_list) 
    | _ -> let gN=6/List.length couleur_list in (*pour remettre le 'N' de l'énoncé*)
        (tourne_config_aux case_list gN,tourne_liste couleur_list);;

  let config_test =
          tourne_config 
          ([((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune);
             ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune)],
            [Jaune]);;

  let rec asso_case_couleur (liste : (int*int*int) list) (j : couleur) : case_coloree list=
      match liste with 
      | [] -> []
      | t :: q -> [(t,j)] @ asso_case_couleur q j;;


(*(-dim-1 ,1, dim)->associer_couleur -> triangle bas -> ajouter dans config -> tourner config -> nbjoueursfois*)

let rec remplir_init (liste_j : couleur list) (dim : int) : configuration =
  match liste_j with
  | [] -> ([], []) (* Pas de joueurs, configuration vide *)
  | joueur :: liste_restante ->
      (* Calcul du triangle de base pour le joueur courant *)
      let case_de_base = remplir_triangle_bas dim (-dim - 1, 1, dim) in
      let triangle_colore = asso_case_couleur case_de_base joueur in
      (* Récupération des configurations des joueurs restants *)
      let (cases_restantes, couleurs_restantes) = remplir_init liste_restante dim in
      (* Rotation des cases restantes pour aligner les bases des joueurs *)
      let angle_rotation = 6 / List.length liste_j in
      let cases_tournees =
        List.map (fun (case, coul) -> (tourne_case angle_rotation case, coul)) cases_restantes
      in
      (* Fusion des résultats *)
      let cases_finales = triangle_colore @ cases_tournees in
      let couleurs_finales = joueur :: couleurs_restantes in
      (cases_finales, couleurs_finales);;

      remplir_init [Jaune; Rouge] 3 ;;



(* Fonction pour afficher une configuration *)
(*
let afficher_configuration (config : configuration) : unit =
  let (case_list, couleur_list) = config in
  (* Affichage des cases colorées *)
  Printf.printf "Cases colorées :\n";
  List.iter (fun ((x, y, z), c) ->
    Printf.printf "((%d, %d, %d), %s)\n"
      x y z
      (match c with
       | Jaune -> "Jaune"
       | Rouge -> "Rouge"
       | Bleu -> "Bleu"
       | Vert -> "Vert")
  ) case_list;
  (* Affichage de la liste des couleurs *)
  Printf.printf "Liste des couleurs :\n";
  List.iter (fun c ->
    Printf.printf "%s\n"
      (match c with
       | Jaune -> "Jaune"
       | Rouge -> "Rouge"
       | Bleu -> "Bleu"
       | Vert -> "Vert")
  ) couleur_list;
  Printf.printf "\n"
;;

let () =
  Printf.printf "Affichage de la configuration après rotation :\n";
  afficher_configuration config_test;;
;;*)

(*il me manque la dernière question -> On passe au math*)


(*Inutile pour le moment
  let rec listeCouleur n = (*il va s'occuper de la liste de couleurs pour la suite*)
    let liste=configuration_initial in  
    match n with
    | 0 -> []
    | _ -> match liste with (_,[]) -> [] | (_, t::q) -> t::listeCouleur (n-1);;

    (*  let remplir_init listeJoueur (dim:dimension) =(*une configuration prend un paramètre une case color list et une liste de couleur*)
(*rajouter les dimensions et reprendre la fonction du dessus*)*)
*)


(*je ne vois pas pour la question 17, je te laisse faire*)


(*question 18*)

let quelle_couleur (c:case) (config:configuration) = 
  if not (est_dans_etoile c) then Dehors (*ancien cas, a garder ?*)
  else
    match List.assoc_opt c (fst config) with (*le fst permet de le matcher avec la première partie de la liste*)
    | None -> Libre
    | Some q -> q;;

(*
    let config = ([((0, 0, 0), Rouge); ((1, -1, 0), Bleu)], [Vert; Jaune; Rouge]);;
    let result = quelle_couleur (0, 0, 0) config;;  (* Devrait retourner Rouge *)
    let result2 = quelle_couleur (0, 1, -1) config;;  (* Devrait retourner Libre *)
    let result3 = quelle_couleur (3, -3, 0) config;;  (*pb avec la config, ce n'est pas la config de base Devrait retourner Dehors, mais il ne renvoie pas le bon ! *)*)

    let liste_joueurs (_, l) = l

    (*q 19*)
let rec supprime_dans_config1 (config:configuration) (c:case) = (*à vérifier*)
  let (listeCase, listeCouleur)=config in
  match listeCase with 
  | [] ->([], listeCouleur)
  | (case, couleur)::q when case=c -> (*on le supprime*) (q, listeCouleur)
  | x::q -> let (l1, l2)=supprime_dans_config1 (q, listeCouleur) c in (x::l1, l2);;

(*q 20*)




(*on passe à la question 21*)





(*on passe à la question 22*)
  
  

let supprime_dans_config (c1 : case) (c2 : case) (config : configuration) : configuration = 
  let (i,j,k) = c1 in 
  let (a,b,c) = c2 in 
  let (case_coloree_list, color_list) = config in 
  let nouvelle_liste = List.filter (fun ((x,y,z), _) -> not ((x = i && y = j && z = k) || (x = a && y = b && z = c))) case_coloree_list in 
  (nouvelle_liste, color_list);;


  let rec verifier_cases_aux (case1 : case)  (vi, vj, vk) remaining_dist case_color_list : bool =
    if remaining_dist = 0 then true 
    else
      let (ci, cj, ck) = case1 in
      if (ci < 0) then 
        if List.exists (fun ((x, y, z), _) ->x = ci && y = cj && z = ck) case_color_list then
          false
        else
          verifier_cases_aux (ci - vi, cj - vj, ck - vk) (vi, vj, vk) (remaining_dist - 1) case_color_list
      else 
      if List.exists (fun ((x, y, z), _) ->x = ci && y = cj && z = ck) case_color_list then
        false
      else
        verifier_cases_aux (ci + vi, cj + vj, ck + vk) (vi, vj, vk) (remaining_dist - 1) case_color_list

  let est_libre_seg (c1 : case) (c2 : case) (config : configuration) : bool =
    let config_new = supprime_dans_config c1 c2 config in
    let ((vi, vj, vk), dist) = vec_et_dist c1 c2 in
    let (case_color_list, _) = config_new in
    (* Appelle la fonction auxiliaire *)
    verifier_cases_aux c1 (vi, vj, vk) dist case_color_list
  ;;













(*Question 24*)
(* est_saut : 
- renvoie faux si la distance dans vec_et_dist est >= 2
- renvoie faux si la case 2 appartient à la config
- renvoie faux si la case entre case1 et case2 n'appartient pas à la config
- renvoie faux si la case2 est en dehors du losange*)

(*on suppose la dimension qu'on a pris en compte au début, peut être faire une fonction pour déterminer la dimension en fonction de la config*)
(*Question 24*)
(* est_saut : 
- renvoie faux si la distance dans vec_et_dist est >= 2
- renvoie faux si la case 2 appartient à la config
- renvoie faux si la case entre case1 et case2 n'appartient pas à la config
- renvoie faux si la case2 est en dehors du losange*)

(*on suppose la dimension qu'on a pris en compte au début, peut être faire une fonction pour déterminer la dimension en fonction de la config*)
let est_saut (c1 : case) (c2 : case) (config : configuration) : bool = (*pour un saut, on appelle celle la*)
  if not(est_dans_losange c2 dim) then false 
  else 
  let (i,j,k) = c2 in
  let ( _ , d) = vec_et_dist c1 c2 in 
  if d > 2 then false
  else 
    let (case_coloree_list, _ ) = config in
    if List.exists (fun ((x, y, z), _) ->x = i && y = j && z = k) case_coloree_list then false
    else true
      (*if est_libre_seg c1 c2 config then false 
      else true*);;

let est_saut_sans_verif_etoile (c1 : case) (c2 : case) (config : configuration) : bool =   (*fonction auxiliaire*)
  let (i,j,k) = c2 in
  let ( _ , d) = vec_et_dist c1 c2 in 
  if d > 2 then false
  else 
    let (case_coloree_list, _ ) = config in
    if List.exists (fun ((x, y, z), _) ->x = i && y = j && z = k) case_coloree_list then false
    else true
      (*if est_libre_seg c1 c2 config then false 
      else true)*);; 

(*Question 25*)
let rec est_saut_multiple (case_list : case list) (config : configuration) : bool = 
      match case_list with 
      | [] -> true
      | t :: [] -> if not(est_dans_losange t dim) then false else est_saut_multiple [] config
      | t1 :: t2 :: q -> if not(est_saut_sans_verif_etoile t1 t2 config) then false 
                          else est_saut_multiple q config;;

  let est_coup_valide (config:configuration) (coup:coup) =
    match coup with
      | Du(c1, c2) -> 
        let couleurC2 =quelle_couleur c2 config in 
        let couleurC1= quelle_couleur c1 config in
        let couleur_joueur = List.hd (liste_joueurs config) in
          if sont_cases_voisines c1 c2 && est_dans_losange c2 dim && couleurC2=Libre && couleurC1=couleur_joueur then true
             (*si elles sont voisines*)
                                (*comment vérifier si il s'agit du pion du joueur ?*)
        else 
          if est_saut c1 c2 config then true
          else false
        | Sm casesListes-> est_saut_multiple casesListes config;;
            
        let rec applique_coup config coup =
          match coup with
          | Du(c1, c2) -> 
              let couleurC1 = quelle_couleur c1 config in
              let (listeCase, listeCouleur) = config in
              (* Supprimer la case c1 et ajouter c2 avec sa couleur *)
              let (nouvelle_listeCase, _) = supprime_dans_config1 config c1 in
              let nouvelle_listeCase = (c2, couleurC1) :: nouvelle_listeCase in
              (nouvelle_listeCase, listeCouleur)
          | Sm casesListes ->
              match casesListes with
              | [] -> config  (* Aucun coup à appliquer *)
              | [x] -> config (* Une seule case, pas de mouvement *)
              | x :: y :: q -> 
                  (* Appliquer un déplacement (Du) entre deux premières cases *)
                  let nouvelle_config = applique_coup config (Du(x, y)) in
                  (* Continuer avec les cases restantes *)
                  applique_coup nouvelle_config (Sm (y :: q))
        


 let mis_a_jour_configuration config coup =(*On doit expliquer l'erreur s'il y en a une*)
    if (est_coup_valide config coup) then let cpt=applique_coup config coup in Ok cpt
    else (*on doit trouver quelle étape a échouer, on reprend juste les cas du dessus*)
    match coup with
        | Du(c1, c2) ->
        let couleurC1=quelle_couleur c1 config in 
           let couleur_joueur=List.hd (liste_joueurs config) in
              if not (couleurC1=couleur_joueur) then Error "Le pion avec lequel on a voulu partir n'appartient pas au joueur"
              else if not (sont_cases_voisines c1 c2) then Error "Les cases ne sont pas voisines"
             else if not (est_dans_losange c2 dim) then Error "La case où on souhaite se déplacer n'appartient pas aux losanges"
             else Error "Ce coup n'est pas valide, la case d'arrivéest occupé"
        | _ -> Error "La liste de case donnée n'est pas bonne";;


        

                  
      let gagnant _ = Libre
      let coup_possible _ _ = []


(*on attend théo pour la suite, on passe aux restes de la partie*)