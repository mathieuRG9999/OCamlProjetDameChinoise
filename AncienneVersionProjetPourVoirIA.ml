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
let gagnant _ = false;;

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
(*
  let test_est_dans_losange = 
    print_string ("le point (0,0,0) est dans le losange de dimension 3 " ^ string_of_bool (est_dans_losange (0,0,0) 3) ^ "       ") (*vrai*);
    print_string ("le point (-3,4,-1) est dans le losange de dimension 3 " ^ string_of_bool (est_dans_losange (-3,4,-1) 3) ^ "       ") (*faux*);
    print_string ("le point (-4,0,0) est dans le losange de dimension 3 " ^ string_of_bool (est_dans_losange (-4,0,0) 3) ^ "     ") (*n'existe pas*);
    print_string ("le point (4,-3,-1) est dans le losange de dimension 3 " ^ string_of_bool (est_dans_losange (4,-3,-1) 3) ^ "       ") (*vrai*);
    print_string ("le point (3,-6,3) est dans le losange de dimension 3 " ^ string_of_bool (est_dans_losange (3,-6,3) 3) ^ "      ") (*faux*);;

*)

  let est_dans_etoile (c:case) =
    let (i, j, k)=c in 
    if  (est_dans_losange (i, j, k) dim || est_dans_losange (j, k, i) dim || est_dans_losange (k,i,j) dim)   then true
    else false;;
 (* 
    let test_est_dans_etoile = 
      print_string ("le point (0,0,0) est dans l'etoile " ^ string_of_bool (est_dans_etoile (0,0,0)) ^ "       ") (*vrai*);
      print_string ("le point (1,0,0) est dans l'etoile " ^ string_of_bool (est_dans_etoile (1,0,0)) ^ "       ") (*false*);
      print_string ("le point (7,-7,0) est dans l'etoile " ^ string_of_bool (est_dans_etoile (7,-7,0)) ^ "       ") (*false*);
      print_string ("le point (-3,4,-1) est dans l'etoile " ^ string_of_bool (est_dans_etoile (-3,4,-1)) ^ "       ") (*vrai*);;
*)


    (*let rec tourne_case m (x:case)=
      let (a, b, c)=x in 
      if m mod 6 =0 then x
      else  tourne_case (m-1) (-c,-a,-b);;
      ne tourne pas dans le sens voulu pour remplir la configuration initiale

      *) 

      let rec tourne_case m (x : case) = 
        let (a,b,c) = x in 
        if m mod 6 = 0 then x 
        else tourne_case (m-1) (-b, -c, -a)
(*
    let test_tourne_case = 
      print_string ("tourner d'un sixième de tour la case (-3,-1,4) renvoie la case " ^ to_string_case (tourne_case 1 (-3,-1,4) ) ) ; (*(-4,3,1)*)
      print_string ("tourner d'un tour de tour la case (-3,-1,4) renvoie la case " ^ to_string_case (tourne_case 6 (-3,-1,4) ) ) ; (*(-3,-1,4)*)
      print_string ("tourner de deux sixième de tour la case (-3,-1,4) renvoie la case " ^ to_string_case (tourne_case 2 (-3,-1,4) ) ) ; (*(-1,4,-3)*);;
*)

    (*Peut être vérifier si les points sont bien dans l'étoile avant et après translation et différence*)

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
(*    let test_diff_case = 
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
(*

    let test_sont_case_voisines = 
      print_string(string_of_bool (sont_cases_voisines (-2,1,1) (-2,2,0)));
      print_string(string_of_bool (sont_cases_voisines (0,0,0) (0,-1,1)));;

*)
    let alignement c1 c2 = (*je ne sais pas encore si cette fonction marchent et si elle est nécessaire*)
      let (x,y,z)=c1 in 
      let (x1,y1,z1)= c2 in 
        if x = x1||y=y1||z=z1 then true
        else false;;



      (*Problème avec calcul pivot voir fonction test*)


    let calcul_pivot case1 case2 = (*on vérifie si les cases sont bien alignés*)(*on pourrait aussi vérifier que les deux éléménts sont dans bien dans le plateau*)
      if case1 = case2 then None
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
    let test_calcul_pivot =
      (*print_string "Calcul du pivot pour (0, -2, 2) et (0, 2, -2) : ";
      match calcul_pivot (0, -2, 2) (0, 2, -2) with
        | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
        | None -> print_string "Pas de pivot";

      print_string "Calcul du pivot pour (0, 0, 0) et (-4, 2, 2) : ";
        match calcul_pivot (0, 0, 0) (-4, 2, 2) with
          | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
          | None -> print_string "Pas de pivot";*)

      print_string "Calcul du pivot pour (-3, 6, -3) et (3, -6, 3) : ";
        match calcul_pivot (-3, 6, -3) (3, -6, 3) with
            | Some (x, y, z) -> print_string ("Pivot : (" ^ string_of_int x ^ " ," ^ string_of_int y ^" ," ^ string_of_int z ^ " )")
            | None -> print_string "Pas de pivot" (*Devrait fonctionner et renvoyer(1,0,-1)*)
 
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

    remplir_triangle_bas 3 (-4,1,3)=
[(-4,1,3);(-4,2,2);(-4,3,1);(-5,2,3);(-5,3,2);(-6,3,3)]

    remplir_triangle_bas 3 (-4,2,2)

*)



(*on passe à la question 14*)

let rec remplir_triangle_haut m (c:case) =(*on force le type pour la simplicité de relecture, il nous faudrait une fonction pour inverser*)
      let (i, j, k)= c in 
    match m with 
    | 0 -> []
    | _ -> (remplir_segment m (i, j, k)) @ remplir_triangle_haut (m-1) (i+1,j, k-1);; 

(*!!!!!!

J ene sais pas de quoi il me parle la : "On donnera d’abord des équations récursives définissant cette fonction formalisant l’idée qu’un triangle est composé d’un segment du bas et du triangle
au-dessus de ce segment"

*)

(*Los testos 
    remplir_triangle_bas 1 (0,0,0);;
    remplir_triangle_bas 3 (-4,1,3);;
*)



(*on passe à la question 15*)


let rec colorie (j : couleur) (liste : case list) : case_coloree list=
  match liste with 
    | [] -> []
    | t :: q -> [(t,j)] @ colorie j q;;

(* q 16 et la 17*)
let rec tourne_config_aux (liste: case_coloree list) angle : case_coloree list= (*On doit tourner de N/36 de ce que j'ai compris*)
    match liste with 
     |[] -> []
      | (case, couleur)::q -> (tourne_case angle case, couleur)::tourne_config_aux q angle;;




  let tourne_config (config:configuration) : configuration= (*A REVOIR*)
    let (case_list, couleur_list)=config in
    match couleur_list with 
    | [] -> (case_list, couleur_list) 
    | _ -> let gN=6/List.length couleur_list in (*pour remettre le 'N' de l'énoncé*)
        (tourne_config_aux case_list gN,tourne_liste couleur_list);;


        let config_test = 
          ([((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune); 
            ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune)], 
           [Jaune; Rouge; Bleu; Vert; Noir; Marron]);;
        
        let config_apres_rotation = tourne_config config_test;;
        
        (* Fonction d'affichage 
        let afficher_configuration (config : configuration) : unit =
          let (case_list, couleur_list) = config in
          Printf.printf "Cases colorées :\n";
          List.iter (fun ((x, y, z), c) ->
            Printf.printf "((%d, %d, %d), %s)\n"
              x y z
              (match c with
               | Jaune -> "Jaune"
               | Rouge -> "Rouge"
               | Bleu -> "Bleu"
               | Vert -> "Vert"
               | Noir -> "Noir"
               | Marron -> "Marron")
          ) case_list;
          Printf.printf "Liste des couleurs :\n";
          List.iter (fun c ->
            Printf.printf "%s\n"
              (match c with
               | Jaune -> "Jaune"
               | Rouge -> "Rouge"
               | Bleu -> "Bleu"
               | Vert -> "Vert"
               | Noir -> "Noir"
               | Marron -> "Marron")
          ) couleur_list;;
        
        (* Affichage *)
        let () =
          Printf.printf "Configuration après rotation :\n";
          afficher_configuration config_apres_rotation;;*)

let liste_joueurs (_, l) = l

(*Question 17*)
(* remplir_init : en fonction de la dimension remplir les cases du bas avec remplir_triangle_bas (-dim-1,1,dim)
et appliquer la couleur puis tourner la config avec tourne_config et recommencer en fonction du nombre de joueur (diminue à chaque tour)

- à chaque itération enlever la couleur du joueur qu'on a appliqué avec enlever_element




*)

let enlever_element element liste =
  List.filter (function x -> x <> element) liste
;;




let rec remplir_init_aux liste_joueurs dim rotation_par_joueur rotation_courante : configuration =
  match liste_joueurs with
  | [] -> ([], []) 
  | joueur :: reste_joueurs ->
      let cases_de_base = remplir_triangle_bas dim (-dim - 1, 1, dim) in
      let cases_tournees =
        List.map (fun case -> (tourne_case rotation_courante case, joueur)) cases_de_base
      in
      let (cases_restantes, couleurs_restantes) =
        remplir_init_aux reste_joueurs dim rotation_par_joueur (rotation_courante + rotation_par_joueur)
      in
      (cases_tournees @ cases_restantes, joueur :: couleurs_restantes)
;;

let remplir_init liste_joueurs dim : configuration =
  let nombre_joueurs = List.length liste_joueurs in
  if nombre_joueurs = 0 || 6 mod nombre_joueurs <> 0 then
    failwith "Le nombre de joueurs doit être un diviseur de 6 (1, 2, 3, 6)."
  else
    let rotation_par_joueur = 6 / nombre_joueurs in
    remplir_init_aux liste_joueurs dim rotation_par_joueur 0
;;


let configuration_initial = remplir_init [Vert; Marron; Bleu;Rouge ;Noir;Jaune] 3;;



(*il me manque la dernière question -> On passe au math*)
  let rec listeCouleur n = (*il va s'occuper de la liste de couleurs pour la suite*)
    let liste=configuration_initial in  
    match n with
    | 0 -> []
    | _ -> match liste with (_,[]) -> [] | (_, t::q) -> t::listeCouleur (n-1);;

    (*  let remplir_init listeJoueur (dim:dimension) =(*une configuration prend un paramètre une case color list et une liste de couleur*)
(*rajouter les dimensions et reprendre la fonction du dessus*)*)


(*question 18*)

let quelle_couleur (c:case) (config:configuration) = 
  if not (est_dans_etoile c) then Dehors 
  else
    match List.assoc_opt c (fst config) with (*le fst permet de le matcher avec la première partie de la liste*)
    | None -> Libre
    | Some q -> q;;

(*
    let config = ([((0, 0, 0), Rouge); ((1, -1, 0), Bleu)], [Vert; Jaune; Rouge]);;
    let result = quelle_couleur (0, 0, 0) config;;  (* Devrait retourner Rouge *)
    let result2 = quelle_couleur (0, 1, -1) config;;  (* Devrait retourner Libre *)
    let result3 = quelle_couleur (3, -3, 0) config;;  (* Devrait retourner Dehors, mais il ne renvoie pas le bon ! problème de config voir question 17*)*)

(*Question 19*)


let rec supprime_dans_config (config:configuration) (c:case) = (*à vérifier*)
  let (listeCase, listeCouleur)=config in
  match listeCase with 
  | [] ->([], listeCouleur)
  | (case, couleur)::q when case=c -> (*on le supprime*) (q, listeCouleur)
  | x::q -> let (l1, l2)=supprime_dans_config (q, listeCouleur) c in (x::l1, l2);;


(*q 20, 21, 22 à la fin*)




(*Question 23*)
(*calculer vec_et_dist c1 c2 puis regarder toutes les cases de la forme n*(i,j,k) et voir si elles appartiennent à la config*)
(*voir pour faire en sorte que si c1 et c2 sont inclus dans la config cela fonctionne quand même*)

let supprime_cases (c1 : case) (c2 : case) (config : configuration) : configuration = 
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
    let config_new = supprime_cases c1 c2 config in
    let ((vi, vj, vk), dist) = vec_et_dist c1 c2 in
    let (case_color_list, _) = config_new in
    (* Appelle la fonction auxiliaire *)
    verifier_cases_aux c1 (vi, vj, vk) dist case_color_list
  ;;






  let config_test_est_libre = 
    ([((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune); 
      ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune)], 
    [Jaune; Rouge; Bleu; Vert; Noir; Marron]);;

est_libre_seg (-6,3,3) (-4,1,3) config_test_est_libre;;


(*Question 24*)
(* est_saut : 
- renvoie faux si la distance dans vec_et_dist est >= 2
- renvoie faux si la case 2 appartient à la config
- renvoie faux si la case entre case1 et case2 n'appartient pas à la config
- renvoie faux si la case2 est en dehors du losange*)

(*on suppose la dimension qu'on a pris en compte au début, peut être faire une fonction pour déterminer la dimension en fonction de la config*)
let est_saut (c1 : case) (c2 : case) (config : configuration) : bool = 
  if not ((est_dans_losange c2 dim)&&(est_dans_losange c1 dim)) then false 
  else 
    let (case_coloree_list, _) = config in
    let ((vi, vj, vk), d) = vec_et_dist c1 c2 in
    if d <> 2 then false
    else
      let (x1, y1, z1) = c1 in
      let (x2, y2, z2) = c2 in
      let pivot = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2) in
      if List.exists (fun (case, _) -> case = pivot) case_coloree_list &&
         not (List.exists (fun (case, _) -> case = c2) case_coloree_list) then
        true
      else
        false;;

        let est_saut_sans_verif_etoile (c1 : case) (c2 : case) (config : configuration) : bool =  
          let (case_coloree_list, _) = config in
          let ((vi, vj, vk), d) = vec_et_dist c1 c2 in
          if d <> 2 then false
          else
            let (x1, y1, z1) = c1 in
            let (x2, y2, z2) = c2 in
            let pivot = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2) in
            if not (est_dans_losange pivot dim) then false
            else if List.exists (fun (case, _) -> case = pivot) case_coloree_list &&
                    not (List.exists (fun (case, _) -> case = c2) case_coloree_list) then
              true
            else
              false;;
        

(*Question 25*)
let rec est_saut_multiple_aux (case_list : case list) (config : configuration) (cases_visitees : case list) : bool =
  match case_list with
  | [] -> true  (* Aucun saut à valider *)
  | [t] -> est_dans_losange t dim  (* Dernière case doit être dans le losange *)
  | t1 :: t2 :: q ->
      (* Vérifie si le saut entre t1 et t2 est valide *)
      let (case_coloree_list, _) = config in
      let config_simulee = (case_coloree_list @ List.map (fun c -> (c, Libre)) cases_visitees, []) in
      if not (est_saut t1 t2 config_simulee) then 
        false
      else
        (* Ajoute t1 aux cases visitées et vérifie les sauts restants *)
        est_saut_multiple_aux (t2 :: q) config (t1 :: cases_visitees);;

let est_saut_multiple (case_list : case list) (config : configuration) : bool =
  est_saut_multiple_aux case_list config [];;







      let est_coup_valide (config:configuration) (coup:coup) =
        match coup with
        | Du(c1, c2) -> 
            let couleurC2 = quelle_couleur c2 config in 
            let couleurC1 = quelle_couleur c1 config in
            let couleur_joueur = List.hd (liste_joueurs config) in
            (* Vérification des conditions pour un déplacement simple *)
            if sont_cases_voisines c1 c2 && est_dans_losange c2 dim && couleurC2 = Libre && couleurC1 = couleur_joueur then true
            else 
              (* Vérification des conditions pour un saut *)
              if est_saut c1 c2 config && couleurC1 = couleur_joueur then true
              else false
        | Sm casesListes -> 
            (* Vérification des conditions pour un saut multiple *)
            if casesListes = [] then false
            else 
              (* Vérifier que le premier élément de la liste correspond à la couleur du joueur *)
              let couleur_premiere_case = quelle_couleur (List.hd casesListes) config in
              let couleur_joueur = List.hd (liste_joueurs config) in
              if couleur_premiere_case <> couleur_joueur then false
              else est_saut_multiple casesListes config;;
      
          
let rec applique_coup config coup =
  match coup with
  | Du(c1, c2) -> 
      let couleurC1 = quelle_couleur c1 config in
      let (listeCase, listeCouleur) = config in
      (* Supprimer la case c1 et ajouter c2 avec sa couleur *)
      let (nouvelle_listeCase, _) = supprime_dans_config config c1 in
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
            else Error "Ce coup n'est pas valide, la case d'arrivée est occupé"
      | _ -> Error "La liste de case donnée n'est pas bonne";;

let gagnant _ = false;;
let coup_possible _ _  = [];;
let ia_next_coup _  = 
    let coup = Sm [] in
    let description = "Coup généré par ia_next_coup" in
        (coup, description);;
