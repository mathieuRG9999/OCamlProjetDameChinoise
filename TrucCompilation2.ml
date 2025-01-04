let dim = 3;;

(*Je pense qu'il faudra le supprimer à la fin*)


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
        else tourne_case (m-1) (-b, -c, -a);;
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


      (*
      tourne_liste [Vert;Jaune;Rouge; Bleu; Marron; Noir];;
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


(*Los testos 
    remplir_triangle_bas 1 (0,0,0);;
    remplir_triangle_bas 3 (-4,1,3);;
*)



(*q 15*)


let rec colorie (j : couleur) (liste : case list) : case_coloree list=
  match liste with 
    | [] -> []
    | t :: q -> [(t,j)] @ colorie j q;;

(* q 16 et la 17*)
let rec tourne_case1 m (x:case)=
let (a, b, c)=x in 
if m mod 6 =0 then x
else  tourne_case1 (m-1) (-c,-a,-b);;

let rec tourne_config_aux (liste: case_coloree list) angle : case_coloree list= (*On doit tourner de N/36 de ce que j'ai compris*)
    match liste with 
     |[] -> []
      | (case, couleur)::q -> (tourne_case1 angle case, couleur)::tourne_config_aux q angle;;




  let tourne_config (config:configuration) : configuration= 
    let (case_list, couleur_list)=config in
    match couleur_list with 
    | [] -> (case_list, couleur_list) 
    | _ -> let gN=6/List.length couleur_list in (*pour remettre le 'N' de l'énoncé*)
        (tourne_config_aux case_list gN,tourne_liste couleur_list);;

        (*

        let config_test = 
          ([((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune); 
            ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune)], 
           [Jaune; Rouge; Bleu; Vert; Noir; Marron]);;
        
        let config_apres_rotation = tourne_config config_test;;
  *)      


let liste_joueurs (_, l) = l;;

(*Question 17*)
(* remplir_init : en fonction de la dimension remplir les cases du bas avec remplir_triangle_bas (-dim-1,1,dim)
et appliquer la couleur puis tourner la config avec tourne_config et recommencer en fonction du nombre de joueur (diminue à chaque tour)

- à chaque itération enlever la couleur du joueur qu'on a appliqué avec enlever_element




*)

let enlever_element element liste =
  List.filter (function x -> x <> element) liste;;

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
      (cases_tournees @ cases_restantes, joueur :: couleurs_restantes);;

let remplir_init liste_joueurs dim : configuration =
  let nombre_joueurs = List.length liste_joueurs in
  if nombre_joueurs = 0 || 6 mod nombre_joueurs <> 0 then
    failwith "Le nombre de joueurs doit être un diviseur de 6 (1, 2, 3, 6)."
  else
    let rotation_par_joueur = 6 / nombre_joueurs in
    remplir_init_aux liste_joueurs dim rotation_par_joueur 0;;

(* On fait des tests*)


let configuration_initial = remplir_init [Vert; Marron] dim;;


(*pour tester si c'est nous qui avons un problème ou eux*)





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
        verifier_cases_aux (ci + vi, cj + vj, ck + vk) (vi, vj, vk) (remaining_dist - 1) case_color_list;;

  let est_libre_seg (c1 : case) (c2 : case) (config : configuration) : bool =
    let config_new = supprime_cases c1 c2 config in
    let ((vi, vj, vk), dist) = vec_et_dist c1 c2 in
    let (case_color_list, _) = config_new in
    (* Appelle la fonction auxiliaire *)
    verifier_cases_aux c1 (vi, vj, vk) dist case_color_list  ;;
(*

  let config_test_est_libre = 
    ([((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune); 
      ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune)], 
    [Jaune; Rouge; Bleu; Vert; Noir; Marron]);;

est_libre_seg (-6,3,3) (-4,1,3) config_test_est_libre;;
*)

(*q 24*)
(*on suppose la dimension qu'on a pris en compte au début, peut être faire une fonction pour déterminer la dimension en fonction de la config*)
let rec case_entre (config : configuration) (c : case) (direction : case) (distance : int) : bool = 
  match distance with 
  | 0 -> false (* Pas de cases intermédiaires *)
  | 1 -> true  (* La dernière case n'est pas une case intermédiaire *)
  | _ -> 
      let (case_coloree_list, _) = config in 
      let (vi, vj, vk) = c in 
      let (zi, zj, zk) = direction in 
      let next_case = (vi + zi, vj + zj, vk + zk) in
      if List.exists (fun (case_coloree, _) -> case_coloree = next_case) case_coloree_list then 
        false (* Case intermédiaire occupée *)
      else 
        case_entre config next_case direction (distance - 1)
;;

(* Calcul de la distance hexagonale entre deux cases *)
let distance_hex c1 c2 =
  let (x1, y1, z1) = c1 in
  let (x2, y2, z2) = c2 in
  (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2
;;




(* Génère une liste de sauts valides depuis case_init dans une direction donnée *)
(* Génère une liste de sauts valides depuis case_init dans une direction donnée *)
let rec calcul_saut (config : configuration) (case_init : case) (case_pivot : case) (direction : case) (dim : int) : coup list = 
  match dim with 
  | 0 -> [] (* Aucun saut disponible *)
  | _ -> 
      let (vi, vj, vk) = case_pivot in 
      let (zi, zj, zk) = direction in 
      let (case_coloree_list, _) = config in 
      (* Vérifie si la case pivot est occupée *)
      if List.exists (fun (case_coloree, _) -> case_coloree = case_pivot) case_coloree_list then 
        let (_, d) = vec_et_dist case_init case_pivot in 
        if d <> 0 then 
          let destination = (vi + d * zi, vj + d * zj, vk + d * zk) in 
          (* Vérifie si le saut est valide : les cases intermédiaires sont libres et destination est libre *)
          if (case_entre config case_pivot direction d) &&
             (not (List.exists (fun (case_coloree, _) -> case_coloree = destination) case_coloree_list)) &&
             (est_dans_etoile destination) then 
            Du (case_pivot, destination) :: calcul_saut config case_init destination direction (dim - 1)
          else 
            calcul_saut config case_init (vi + zi, vj + zj, vk + zk) direction (dim - 1)
        else 
          calcul_saut config case_init (vi + zi, vj + zj, vk + zk) direction (dim - 1)
      else 
        (* Continue la recherche en avançant dans la direction *)
        calcul_saut config case_init (vi + zi, vj + zj, vk + zk) direction (dim - 1)
;;




let config_test = ([((-3, 2, 1), Vert); ((0, 2, -2), Vert)], [Vert; Jaune]);;

calcul_saut config_test (-3, 2, 1) (-2, 2, 0) (1, 0, -1) 2;;






let est_saut ((x1, y1, z1) : case) ((x2, y2, z2) : case) (config : configuration) : bool = 
  if not ((est_dans_losange (x2, y2, z2) dim)&&(est_dans_losange (x1, y1, z1) dim)) then false 
  else 
    let (case_coloree_list, _) = config in
    let (_, d) = vec_et_dist (x1, y1, z1) (x2, y2, z2) in 
    if d mod 2 = 1 then false else 
      let pivot = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2) in
      if List.exists (fun (case, _) -> case = pivot) case_coloree_list &&
          not (List.exists (fun (case, _) -> case = (x2, y2, z2)) case_coloree_list) then
        let ((xp1, yp1, zp1), d1) = vec_et_dist (x1, y1, z1) pivot in
        let ((xp2, yp2, zp2), d2) = vec_et_dist pivot (x2, y2, z2) in 
        if case_entre (x1, y1, z1) (xp1, yp1, zp1) config d1 && case_entre pivot (xp2, yp2, zp2) config d2 then true
        else false
      else
        false;;


let est_saut_sans_verif ((x1, y1, z1) : case) ((x2, y2, z2) : case) (config : configuration) : bool = 
    let (case_coloree_list, _) = config in
    let ((vi, vj, vk), d) = vec_et_dist (x1, y1, z1) (x2, y2, z2) in 
    if d mod 2 = 1 then false else 
      let pivot = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2) in
      if List.exists (fun (case, _) -> case = pivot) case_coloree_list &&
          not (List.exists (fun (case, _) -> case = (x2, y2, z2)) case_coloree_list) then
            let ((xp1, yp1, zp1), d1) = vec_et_dist (x1, y1, z1) pivot in
            let ((xp2, yp2, zp2), d2) = vec_et_dist pivot (x2, y2, z2) in 
            if case_entre (x1, y1, z1) (xp1, yp1, zp1) config d1 && case_entre pivot (xp2, yp2, zp2) config d2 then true
            else false
      else
        false;;


(*Question 25*)
(*on ne doi tpas vérifier si la premiere et la dernière case sont bien dans le losange*)
(*on va devoir le vérifier, dans "est_coup_valide"*)
let rec est_saut_multiple_aux (case_list : case list) (config : configuration) (cases_visitees : case list) : bool =
  match case_list with
  | [] -> true  (* Aucun saut à valider *)
  | [t] -> est_dans_losange t dim  (* Dernière case doit être dans le losange *)
  | t1 :: t2 :: q ->
      (* Vérifie si le saut entre t1 et t2 est valide *)(*on doit aussi vérifier si les deux cases sont livres*)
      let (case_coloree_list, _) = config in
      let config_simulee = (case_coloree_list @ List.map (fun c -> (c, Libre)) cases_visitees, []) in
(*on vérifie que les deux couleurs sont bonnes*)
       if not (est_saut_sans_verif t1 t2 config_simulee) then 
        false
      else
        (* Ajoute t1 aux cases visitées et vérifie les sauts restants *)
        est_saut_multiple_aux (t2 :: q) config (t1 :: cases_visitees);;

let est_saut_multiple (case_list : case list) (config : configuration) : bool =
  est_saut_multiple_aux case_list config [];;



let premierListe liste = (*on sait que la liste n'est pas vide*)
  match liste with
  | t::q -> t;;

let  rec dernierListe liste =
  match liste with
  | t::[]-> t
  | t::q -> dernierListe q;;

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
              let cPremier =premierListe casesListes in
              let cDernier=dernierListe casesListes in 
              if (est_dans_losange cPremier dim && est_dans_losange cDernier dim && quelle_couleur cDernier config = Libre) then 
                (* Vérifier que le premier élément de la liste correspond à la couleur du joueur *)
                let couleur_premiere_case = quelle_couleur (List.hd casesListes) config in
                let couleur_joueur = List.hd (liste_joueurs config) in
                if couleur_premiere_case <> couleur_joueur then false
                else est_saut_multiple casesListes config
              else false;;
      
          
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
          applique_coup nouvelle_config (Sm (y :: q));;
      


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


            

(*troisième partie*)
  

(*pour calculer le score, il suffit de sommer les (i) de chacun de ses valeurs*)




(*La partie que théo vient de m'envoyer*)
let rec score_aux (config:configuration) couleur = (*on cherche les éléments dans la configuration ayant cette valeur*)
          match fst config with (*permet d'avoir la première partie =first*)
          | [] -> 0
          | ((case, coul)::q) when coul=couleur -> let (i, j, k)=case in i+score_aux (q, snd config) couleur (*=second*)
          | (_::q) -> score_aux (q, snd config) couleur;;
          
          
          (*q 27*)
let score config =
  match liste_joueurs config with
   | [] -> 0 (* la liste est vide donc ne contient rien*)
   | couleur_protagoniste::_ ->score_aux config couleur_protagoniste;;

let score_gagnant (dim :int) : int = 
  let list_c = remplir_triangle_haut dim (dim+1, -dim,-1) in 
  let liste_c_coul = colorie Jaune list_c in 
  let config_test_score = (liste_c_coul, [Jaune]) in 
  score config_test_score;;




let config_test = 
  ([((4, -1, -3), Jaune); ((4, -2, -2), Jaune); ((4, -3, -1), Jaune); 
    ((5, -2, -3), Jaune); ((5, -3, -2), Jaune); ((6, -3, -3), Jaune)], 
   [Jaune; Rouge; Bleu; Jaune; Noir; Marron]) ;;

let liste_couleur_test = [Vert; Rouge; Bleu; Jaune; Noir; Marron]

(*let rec score_total config liste = (*il s'agit de la liste des couleurs de tous les joueurs*)
match liste with
| [] -> []
| t::q->   let score_t = score config in let cf = tourne_config config in  score_t :: score_total cf q;;      (*cf devrait être de type config et on aurait pas le soucis*)                   

(*il faut tourner le plateau et faire appel à l'autre fonction *)*)




(*q 28*)
(*let rec gagnant_aux max liste=
match liste with 
| []-> false
| [x] when x>max -> true
| [x] when x<max -> false
| t::q -> if (t>max) then gagnant_aux t q else gagnant_aux max q;;*)

(*je ne sais pas à quoi ces deux fonction servent*)

(*ETRE SUR QUE CES FONCTIONS MARCHENT*)

let guess_dim (config : configuration) = let (liste_case_coloree,_) = config in 
let dim = List.fold_left (fun acc ((i, _, _), _) -> max acc i) min_int liste_case_coloree in dim/2;;


guess_dim configuration_initial;;

let gagnant (config : configuration) : bool = 
  if score config < score_gagnant (guess_dim configuration_initial) then false 
  else true;;


(*let gagnant (config:configuration) = gagnant_aux 0 (score_total config (liste_joueurs config))*) ;;






(*ma partie à moi*)

(*let rec score_aux (config:configuration) couleur = (*on cherche les éléments dans la configuration ayant cette valeur*)
          match fst config with (*permet d'avoir la première partie =first*)
          | [] -> 0
          | ((case, coul)::q) when coul=couleur -> let (i, j, k)=case in i+score_aux (q, snd config) couleur (*=second*)
          | (_::q) -> score_aux (q, snd config) couleur;;
          
          
          (*q 27*)
let score config =
  match liste_joueurs config with
   | [] -> 0 (* la liste est vide donc ne contient rien*)
   | couleur_protagoniste::_ ->score_aux config couleur_protagoniste;;

   score configuration_initial;;

   
  let rec score_total config liste = (*il s'agit de la liste des couleurs de tous les joueurs*)
    match liste with
    | [] -> []
    | t::q->   let score_t = score config in let cf = tourne_config config in  score_t :: score_total cf q;;      (*cf devrait être de type config et on aurait pas le soucis*)                                              (*il faut tourner le plateau et faire appel à l'autre fonction *)



(*q 28*)
    let rec gagnant_aux max liste=
    match liste with 
    | []-> false
    | [x] when x>max -> true
    | [x] when x<max -> false
    | t::q -> if (t>max) then gagnant_aux t q else gagnant_aux max q;;

let gagnant (config:configuration) = gagnant_aux 0 (score_total config (liste_joueurs config)) ;;

gagnant configuration_initial;;*)

let directions = [ (1, -1, 0); (-1, 1, 0);
 (1, 0, -1); (-1, 0, 1); (0, 1, -1); (0, -1, 1) ] ;; (*il s'agit de toutes les directions dans lequel le joueur peut aller*)


(*pour les coups uniques*)


(*j'ai inversé les ordres des fonctions auxiliares à coup_possible afin de clarifier et de retourner aux normes*)

let listeDeplacementUnitaire_aux (config: configuration) (caseInitial: case) directions : (coup * case) list = (*essayer de simplifier cette fonction*)
  List.fold_left (fun acc direction ->
    let caseSuivante = translate caseInitial direction in
    let coup = Du(caseInitial, caseSuivante) in
    if est_coup_valide config coup then
      (coup, caseSuivante) :: acc
    else
      acc) [] directions;;

let listeDeplacementUnitaire (config: configuration) (caseInitial: case) : (coup * case) list =
  listeDeplacementUnitaire_aux config caseInitial directions;;


listeDeplacementUnitaire configuration_initial (-4,3,1)

(*Le tri faut le faire ensuite ou maintenant ?*)

(*on cherche les erreurs dans le code*)


(*on l'a encore changé v1*)
(*
let rec listeDeplacementMultiple_aux (config: configuration) (caseInitial: case) (listeCasesUtilisees: case list) directions : (case * coup) list =
  directions
  |> List.fold_left (fun acc direction ->
       let caseSuiv = translate caseInitial direction in
       let couleurSuiv = quelle_couleur caseSuiv config in
       if couleurSuiv <> Libre && not (List.mem caseSuiv listeCasesUtilisees) then
         let caseSuivSuiv = translate caseSuiv direction in
         let couleurSuivSuiv = quelle_couleur caseSuivSuiv config in
         if couleurSuivSuiv = Libre && est_dans_etoile caseSuivSuiv && not (List.mem caseSuivSuiv listeCasesUtilisees) then
           (* Construire le nouveau coup *)
           let nouveauCoup = Sm (List.rev (caseSuivSuiv :: caseInitial :: listeCasesUtilisees)) in
           (* Continuer la recherche récursive *)
           let nouvelles_directions = listeDeplacementMultiple_aux config caseSuivSuiv (caseSuivSuiv :: listeCasesUtilisees) directions in
           (* Ajouter les nouveaux coups trouvés *)
           (caseSuivSuiv, nouveauCoup) :: nouvelles_directions @ acc
         else
           acc (* Pas de saut possible *)
       else
         acc) [];;
*)
(*nouvelle version*)
let rec listeDeplacementMultiple_aux (config: configuration) (caseInitial: case) (listeCasesUtilisees: case list) directions : (coup * case) list =
  directions
  |> List.fold_left (fun acc direction ->
       let caseSuiv = translate caseInitial direction in
       let couleurSuiv = quelle_couleur caseSuiv config in
       if couleurSuiv <> Libre && not (List.mem caseSuiv listeCasesUtilisees) then
         let caseSuivSuiv = translate caseSuiv direction in
         let couleurSuivSuiv = quelle_couleur caseSuivSuiv config in
         if couleurSuivSuiv = Libre && est_dans_etoile caseSuivSuiv && not (List.mem caseSuivSuiv listeCasesUtilisees) then
           let nouveauCoup = Sm (listeCasesUtilisees @ [caseSuivSuiv]) in
           if est_coup_valide config nouveauCoup then
             let nouvelles_directions = listeDeplacementMultiple_aux config caseSuivSuiv (listeCasesUtilisees @ [caseSuivSuiv]) directions in
             (nouveauCoup, caseSuivSuiv) :: nouvelles_directions @ acc
           else
             acc
         else
           acc
       else
         acc) [];;


let listeDeplacementMultiple (config: configuration) (caseInitial: case) : (coup * case) list =
  listeDeplacementMultiple_aux config caseInitial [caseInitial] directions;;


(*Premier test de fonctions*)


(* Fonction pour appliquer une translation multiple le long d'une direction *)
































































(*ancinee verions du la fonction qui appel l'auxiliaire*)


   (*
let listeDeplacementMultiple (config: configuration) (caseInitial: case) : (case * coup) list =
  listeDeplacementMultiple_aux config caseInitial [] directions;;
*)

let test =listeDeplacementMultiple configuration_initial (-5,3,2);;



(*on crée une configuration avec sauts multiples pour vérifier que ça fonctionne*)

configuration_initial;;

let configSautMultiple = (*on a un saut multiple pour le premier vert qui est possible*)
  ([((-4, 2, 2), Vert); ((-3, 2, 1), Vert); ((-3, 1, 2), Vert);
  ((-2, 1, 1), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert);
  ((4, -1, -3), Marron); ((4, -2, -2), Marron); ((4, -3, -1), Marron);
  ((5, -2, -3), Marron); ((5, -3, -2), Marron); ((6, -3, -3), Marron)],
 [Vert; Marron]);;


listeDeplacementMultiple configSautMultiple (-4,2,2);;


(*(*Test*)
  listeDeplacementMultiple configuration_initial (5,-2,-3);;

        (*on simplifie l'appelle de cette fonction*)
*)



 (*pourquoi il peut y avoir une infinité de coup ? si on effectue différent coup toutes les cases du losange sont atteignables ?*)
(*on doit renvoyer une liste de coup possible, fonction clé*)


let coup_possibles (config:configuration) (caseInitial:case) : (coup*case) list= (*avec un s ou pas ?*)
  let listeCoupUnique=listeDeplacementUnitaire config caseInitial in
  let listeCoupPlusieurs=listeDeplacementMultiple config caseInitial in 
   listeCoupUnique  @ listeCoupPlusieurs ;;

(*on vérifie que cette fonction marche*)
coup_possibles configuration_initial (-6,3,3);; (*tourne en boucle pour le moment*)

coup_possibles configuration_initial (0,0,0);; (*est ce que ca fais ce qui est attendu ? comment pourrais-je le vérifier ?*)
(*q 30 -Avant de faire quoique ce soit de compliqué, on va prendre une fonction qui choisit au hasard un pion
  -> il faut seulement qu'il y ait un coup possible et elle l'appliquera

  -> On regarde ensuite comment on peut choisir l'option qui permet d'augmenter le plus le score


*)

(*au dessus; cela fonctionne*)

let coup_possible config case =coup_possibles config case;; (*problème avec le truc du prof*)

let rec case_bonne_couleur (config:configuration) = (*fonctionne*)
  let couleur=List.hd (snd config) in 
  match fst config with
  | ([]) -> []
  | ((case, coul)::q) -> 
    if coul=couleur then case::case_bonne_couleur (q, snd config) else case_bonne_couleur(q, snd config);;

 (*
    let rec prendreElementAN liste n = (*on suppose que la liste n'est pas vide, on accepte la surlignance jaune*)
    match liste with
    | t::q -> 
      match n with
      |  0 -> t
      | z -> prendreElementAN q (z-1) ;;


      let next_coup_ia1 (config: configuration) : coup = (* cette fonction renvoie un coup valide que le joueur peut faire *)
      let listeCasesPossibles = case_bonne_couleur config in (* on a toutes les cases sur lesquelles on peut effectuer quelque chose *)
      let nombreHasardPourCase = Random.int (List.length listeCasesPossibles) in 
      let caseChoisie = prendreElementAN listeCasesPossibles nombreHasardPourCase in
      let listeCoup = List.map snd (coup_possibles config caseChoisie) in (* extrait les coups possibles de la case choisie *)
      let nombreHasardPourCoup = Random.int (List.length listeCoup) in 
      prendreElementAN listeCoup nombreHasardPourCoup ;;(* retourne un élément de type `coup` parmi les coups possibles *)
    
(*on teste notre premier IA*)

(*a l'air de fonctionner*)
let test = let coup =next_coup_ia1 configuration_initial in applique_coup configuration_initial coup;;

*)

      (*
let next_coup_ia1 (config:configuration) :coup = (*cette fonction renvoie un coup valide que le joueur peut faire*)
    let listeCasesPossibles=case_bonne_couleur config in (*on a toutes les cses sur lequel on peut effectuer qqch*)
    let nombreHasardPourCase = Random.int (List.length  listeCasesPossibles) in 
    let caseChoisie=prendreElementAN listeCasesPossibles nombreHasardPourCase in
    let listeCoup =snd ( coup_possible config caseChoisie) in
    let nombreHasardPourCoup = Random.int (List.length listeCoup) in
    prendreElementAN listeCoup nombreHasardPourCoup;;*)




(*Est ce que maintenant on peut essayer de prendre le mouvement qui agrandit le plus notre score ?, on fera ensuite l'algo de MinMax puis on ajoutera l'"Alpha-Beta Prunning" ? *)

(*on suppose connnaitre la case et on essaie de voir le coup qui augmente le plus le score de ce joueur*)

(*Ce serait sympa si elle me renvoyait la valeur d'augmentation pour que je puisse choisir quelle est la meilleure case*)

(*


let rec meilleurCaseAChoisir_aux (config:configuration) (valeurMeilleurcoup:int) (meilleurCoup:coup) listesCasesPossible : coup =
  match listesCasesPossible with 
  | [] -> meilleurCoup
  | casePossibles::casesRestantes -> 
    
    let scoreInitial=score config in 
    let listeCoup =List.map snd (coup_possibles config casePossibles) in

    let coup_defaut = match listeCoup with
    | [] -> meilleurCoup (*il n'y en a pas de dispo*)
    | premierCoup::_ -> premierCoup 
    in
    let (coup, score)=meilleurCoupSurLaCase config casePossibles scoreInitial min_int coup_defaut listeCoup in 
      if (score>valeurMeilleurcoup) then meilleurCaseAChoisir_aux config score coup casesRestantes 
      else  meilleurCaseAChoisir_aux config valeurMeilleurcoup meilleurCoup casesRestantes and 


  meilleurCoupSurLaCase (config:configuration) (caseInitial:case) (scoreInitial:int) (maxAugmentationScore:int) (meilleurCoup1:coup) (listeCoup): (coup*int) =
      match listeCoup with 
      | [] -> (meilleurCoup1, maxAugmentationScore)
      | coup::listeRestanteDeCoup ->
        let configPossible= applique_coup config coup in let augmentationDeCeCoup=(score configPossible)-scoreInitial in 
          if augmentationDeCeCoup>maxAugmentationScore then meilleurCoupSurLaCase config caseInitial scoreInitial augmentationDeCeCoup coup listeRestanteDeCoup
          else meilleurCoupSurLaCase config caseInitial scoreInitial maxAugmentationScore meilleurCoup1 listeRestanteDeCoup;;
*)


(*je ne sais plus ce que je voulais faire ici *)



(*on commence à l'implémenter et ensuite, on regarde la suite des probas*)

let rec tous_les_coups_possibles_aux (config:configuration) liste = (*nous renvoie tous les coups possibles*)(*renvoie la case à laquelle on applique une liste*)
      match liste with 
      | [] -> []
      | case::listesDeCases -> coup_possibles config case@ tous_les_coups_possibles_aux config listesDeCases;;

let tous_les_coups_possibles (config:configuration) = tous_les_coups_possibles_aux config (case_bonne_couleur config);;


tous_les_coups_possibles configuration_initial;;


(*on teste dans le cas des sauts multiples*)


tous_les_coups_possibles configSautMultiple;;


(*on va trier les coups pour voir *)
(*
let rec bubbleSortScore (liste:(coup*case) list) (config:configuration) (nb:int)=(*on trie en fonction du score*)
    match liste with 
    | [] -> if nb=0 then [] else bubbleSortScore liste config (nb-1)
    | x::[] ->[x]
    | couP1::couP2::reste ->
      let (coup1, _ )=couP1 in let (coup2, _)=couP2 in
      let config1=applique_coup config coup1 in
      let config2=applique_coup config coup2 in
      let score1=score config1 in
      let score2= score config2 in
      if (score2>score1) then couP2::bubbleSortScore (couP1::reste) config nb
      else couP1::bubbleSortScore (couP2::reste) config nb;;



*)

(*on va tester si tous els coups possibles fonctionne*)

















(*On a sur le github le fichier, on peut essayer de le "prunner" avec un Alpha et un Beta*)

(*Ancienne version de l'algo minMax, je en savais pas ce qui genait*)
(*
let evaluer_coup (config : configuration) (coup : coup) (historique : configuration list) : int =
  if not (est_coup_valide config coup) then  (*je ne suis pas sensé avoir ce cas la enfaite*)
    (* Grosse pénalité pour signaler que ce coup est "mauvais/invalide" *)
    -9999
  else
    let nouvelle_config = applique_coup config coup in
    if List.mem nouvelle_config historique then
      (* Si déjà visitée, pénalité plus modérée *)
      -1000
    else
      (* Sinon, calcul normal : différence de score entre la config finale et la config initiale *)
      score nouvelle_config - score config;;
*)
(* Une version de score paramétrée par la couleur. *)
let score_joueur (config: configuration) (couleur: couleur) : int =
  let (listeCase, _) = config in
  let rec aux l acc =
    match l with
    | [] -> acc
    | ((c_i, c_j, c_k), coul) :: q ->
       if coul = couleur then aux q (acc + c_i)  (* ou un autre calcul *)
       else aux q acc
  in aux listeCase 0;;

  






(* Fonction pour déterminer le prochain coup de l'IA *)



(*on va faire une heuristique pour pouvoir avoir le meilleur coup plus rapidement*)






(*est ce que la fonction du dessous fonctionne ? Est ce qu'elle permet d'améliorer notre profondeur ? *)
let nbSaut coup =
  match coup with
  | Du (_, _) -> 1
  | Sm liste -> List.length liste -1;;

(*il faut aussi que je m'assure que personne ne reste en arrière et que tout le monde monte*)

let pions_en_retards (config : configuration) (couleur : couleur) =
  let (cases, _) = config in
  let coord_joueur = List.filter (fun ((x, y, z), c) -> c = couleur) cases in
  match coord_joueur with
  | [] -> 0
  | l ->
      let minX =
        List.fold_left
          (fun acc ((x, _, _), _) -> if x < acc then x else acc)
          max_int
          l
      in
      minX;;




  let heuristique config joueur_max coups = (*cout O(nlog(n))*) (*supprimer les coups qui font baisser le score  en tout cas ce clairement mauvais?*)
    let score_initial= score config in
    (*on élimine les coups qui nous font reculer*)
    let liste =List.filter (fun (coup,_) -> score (applique_coup config coup)+1>score_initial ) coups in (*on pourrait en fonction du nombre de coups réduire ou augmenter le filtre*)


    List.fast_sort (*on regarde le fast_sort qui apparement est plus rapide*)
      (fun (coup1, _) (coup2, _) ->
        (* On applique chaque coup pour calculer son score *)
        let configParallele1 = applique_coup config coup1 in
        let configParallele2 = applique_coup config coup2 in
        let score1 = score configParallele1 in
        let score2 = score configParallele2 in
  
        (* On calcule le nb de sauts pour chaque coup *)
        let nbSaut1 = nbSaut coup1 in
        let nbSaut2 = nbSaut coup2 in
  
        (* On compare d’abord par score (ordre décroissant) *)
        let c_score = compare score2 score1 in
        if c_score <> 0 then
          c_score
        else
          (* Si les scores sont égaux, on compare par nb de sauts (ordre décroissant) *)
          compare nbSaut2 nbSaut1
      )
      liste;;


    

    


(*l'IA n'arrive pas à finir même si elle a l'avantage, peut etre changer dans l'algo min max les conditions de choix*)
(*est ce qu'il s'agit d'un coup valide dans el cas W vs BI ? est ce qu'on a mal fait notre coup valide*)


    heuristique configuration_initial Vert (tous_les_coups_possibles configuration_initial);;








(*
let rec algoMinMax (config: configuration) (profondeur: int) (alpha: int) (beta: int)
                  (joueur_max: couleur) (joueurs: couleur list) : (int * coup option) =
  let joueur_actuel = List.hd joueurs in

  (* Condition d'arrêt *)
  if profondeur = 0 || gagnant config then
    (score_aux config joueur_max, None)
  else
    let  coups_possibles =List.map fst ( tous_les_coups_possibles config ) in
    if coups_possibles = [] then
      (score_aux config joueur_max, None)
    else
      match joueur_actuel with
      | couleur when couleur = joueur_max ->
          (* Phase de Maximisation *)
          let rec max_value coups acc_alpha acc_beta best_score best_coup =
            match coups with
            | [] -> (best_score, best_coup)
            | coup :: reste ->
                let nouvelle_config = applique_coup config coup in
                let (score, _) = algoMinMax nouvelle_config (profondeur - 1) acc_alpha acc_beta joueur_max (List.tl joueurs @ [List.hd joueurs]) in
                if score > best_score then
                  let new_alpha = max acc_alpha score in
                  if new_alpha >= acc_beta then
                    (new_alpha, Some coup)  (* Élagage *)
                  else
                    max_value reste new_alpha acc_beta score (Some coup)
                else
                  if acc_alpha >= acc_beta then
                    (acc_alpha, best_coup)  (* Élagage *)
                  else
                    max_value reste acc_alpha acc_beta best_score best_coup
          in
          max_value coups_possibles alpha beta min_int None
      | _ ->
          (* Phase de Minimisation *)
          let rec min_value coups acc_alpha acc_beta best_score best_coup =
            match coups with
            | [] -> (best_score, best_coup)
            | coup :: reste ->
                let nouvelle_config = applique_coup config coup in
                let (score, _) = algoMinMax nouvelle_config (profondeur - 1) acc_alpha acc_beta joueur_max (List.tl joueurs @ [List.hd joueurs]) in
                if score < best_score then
                  let new_beta = min acc_beta score in
                  if acc_alpha >= new_beta then
                    (new_beta, Some coup)  (* Élagage *)
                  else
                    min_value reste acc_alpha new_beta score (Some coup)
                else
                  if acc_alpha >= acc_beta then
                    (acc_beta, best_coup)  (* Élagage *)
                  else
                    min_value reste acc_alpha acc_beta best_score best_coup
          in
          min_value coups_possibles alpha beta max_int None;; *)

(*on doit régler le problème des fins de partie*)


(*faire un endgame solver*)

let rec somme k n =
  match k with
  | k when k=n ->0
  | _ -> k +somme (k+1) n;;



  (*on va dire qu'on rentre en fin de jeu lorsque j'ai deja j'ai 4 élements qui sont déja le triangle bien intallé et je ne devrais plus toucher à eux*)

(*on va aussi essayer de tous les pousser vers l'avant de sorte que personne ne soit laissez derrière*)


        

(*test avec une nouvelle fin de partie*)

(*
  let est_fin_de_partie config joueur_max nombreDeCasesVidesAttendu= (*on vérifie que toutes les cases au dessus (strict) de dim+1 sont remplis avec nos cases et à n+1 il reste n cases à remplir*)
    let listeTriangle =remplir_triangle_haut dim (dim, -dim,-1) in
    let test1 = List.for_all (fun c -> 
        match quelle_couleur c config with
        | couleur when couleur=joueur_max -> true
        | _ -> false
      ) (recuperer_n_ieme listeTriangle nombreDeCasesVidesAttendu)
    in 
      if not (test1) then false
      else let listeTriangle1 = take (min nombreDeCasesVidesAttendu dim) listeTriangle  in
      nCasesVides config listeTriangle1 joueur_max 0 nombreDeCasesVidesAttendu  ;;

est_fin_de_partie config_test Vert 2;;
*)

let rec extraire_case (liste_case : case list) n =
  match liste_case with
  | [] -> []
  | q :: t ->let (x,y,z) = q in if x > dim+1 then liste_case else extraire_case t n 


let est_fin_de_partie config joueur_max dim =
  let base_triangle = remplir_triangle_haut dim (dim + 1, -dim, -1) in
  if List.length base_triangle < dim then false else
  let dernieres_cases = extraire_case base_triangle dim in 
  List.for_all (fun case -> quelle_couleur case config = joueur_max) dernieres_cases;;
















let rec algoMinMax1 (config: configuration) (profondeur: int) (alpha: int) (beta: int) (joueur_max: couleur) (joueurs: couleur list) : (int * coup option) =

  let joueur_actuel = List.hd joueurs in
  
  (* Condition d'arrêt *)
  if profondeur = 0 || gagnant config then
    (score_aux config joueur_max, None)
  else
    let  coups_possibles =List.map fst (heuristique config joueur_max (tous_les_coups_possibles config) ) in
    if coups_possibles = [] then
      (score_aux config joueur_max, None)
    else
      match joueur_actuel with
      | couleur when couleur = joueur_max ->
      (* Phase de Maximisation *)


      let rec max_value coups acc_alpha acc_beta best_score best_coup =
      match coups with
        | [] -> (best_score, best_coup)
        | coup :: reste ->
          let nouvelle_config = applique_coup config coup in

          (*ce que je viens de rajouter*)
          if gagnant nouvelle_config then (max_int, Some coup) else
          


          (*fin du rajout*)

          let (score, _) = algoMinMax1 nouvelle_config (profondeur - 1) acc_alpha acc_beta joueur_max (List.tl joueurs @ [List.hd joueurs]) in
          if score > best_score then
            let new_alpha = max acc_alpha score in
            if new_alpha >= acc_beta then
              (new_alpha, Some coup)  (* Élagage *)
            else
              max_value reste new_alpha acc_beta score (Some coup)
          else
            if acc_alpha >= acc_beta then
              (acc_alpha, best_coup)  (* Élagage *)
            else
              max_value reste acc_alpha acc_beta best_score best_coup
            in
           max_value coups_possibles alpha beta min_int None
| _ ->
  (* Phase de Minimisation *)
  let rec min_value coups acc_alpha acc_beta best_score best_coup =
    match coups with
    | [] -> (best_score, best_coup)
    | coup :: reste ->
        let nouvelle_config = applique_coup config coup in

        (*ce que je viens de rajouter*)
        if gagnant nouvelle_config then (min_int, Some coup) else 
        (*fin du rajout*)


        let (score, _) = algoMinMax1 nouvelle_config (profondeur - 1) acc_alpha acc_beta joueur_max (List.tl joueurs @ [List.hd joueurs]) in
        if score < best_score then
          let new_beta = min acc_beta score in
          if acc_alpha >= new_beta then
            (new_beta, Some coup)  (* Élagage *)
          else
            min_value reste acc_alpha new_beta score (Some coup)
        else
          if acc_alpha >= acc_beta then
            (acc_beta, best_coup)  (* Élagage *)
          else
            min_value reste acc_alpha acc_beta best_score best_coup
           in
           min_value coups_possibles alpha beta max_int None;;






(*test potentiel avec un endgame solver de ici*)



let rec endgame_solver_aux config joueur_max profondeur joueur_actuel : (int * coup option) =
  if profondeur = 0 || gagnant config then
    (* Évaluation finale pour le joueur max *)
    let score_actuel = score_aux config joueur_max in
    (score_actuel, None)
  else
    let coups_possibles = List.map fst (tous_les_coups_possibles config) in
    if coups_possibles = [] then
      (* Aucun coup possible *)
      (score_aux config joueur_max, None)
    else
      let evaluer_coup coup =
        let nouvelle_config = applique_coup config coup in
        (* Appel récursif pour simuler les coups adverses *)
        let (score, _) = endgame_solver_aux nouvelle_config joueur_max (profondeur - 1) (List.hd (List.tl (liste_joueurs config))) in
        score
      in
      (* Maximisation pour le joueur max, minimisation pour l'adversaire *)
      let comparer_scores (score1, _) (score2, _) =
        if joueur_actuel = joueur_max then compare score2 score1
        else compare score1 score2
      in
      let meilleurs_coups =
        List.fold_left
          (fun acc coup ->
             let score_coup = evaluer_coup coup in
             (score_coup, Some coup) :: acc)
          []
          coups_possibles
      in
      (* Retourne le meilleur coup en fonction du joueur actuel *)
      List.hd (List.sort comparer_scores meilleurs_coups);;

let endgame_solver config joueur_max profondeur =
  let joueur_actuel = List.hd (liste_joueurs config) in
  snd (endgame_solver_aux config joueur_max profondeur joueur_actuel);;


(* Génère toutes les séquences de coups possibles à partir d'une configuration donnée *)
let rec explorer_chemins config joueur_max profondeur chemin_actuel chemins_valides =
  if profondeur = 0 then chemins_valides
  else
    let coups_possibles = List.map fst (tous_les_coups_possibles config) in
    List.fold_left
      (fun acc coup ->
         let nouvelle_config = applique_coup config coup in
         if gagnant nouvelle_config then
           (* Si on trouve une solution, on ajoute le chemin *)
           (List.rev (coup :: chemin_actuel)) :: acc
         else
           (* Sinon, on continue d'explorer *)
           explorer_chemins nouvelle_config joueur_max (profondeur - 1) (coup :: chemin_actuel) acc)
      chemins_valides
      coups_possibles

(* Fonction principale pour résoudre la fin de partie *)
let solver_fin_de_partie config joueur_max profondeur =
  let chemins = explorer_chemins config joueur_max profondeur [] [] in
  match chemins with
  | [] -> None (* Aucune solution trouvée *)
  | solutions ->
      (* Retourne la première séquence de coups menant à la victoire *)
      let meilleur_chemin = List.hd (List.sort (fun a b -> compare (List.length a) (List.length b)) solutions) in
      Some (List.hd meilleur_chemin) (* Premier coup du meilleur chemin *)




(*jusque la*)






(*sur la fin on a toujours le soucis, on peut filter s'il est à la fin, on met celui la a la fin. Faire un A ou un Djistrka pour retrouver la position dans le cas ou il n'en reste pas beaucoup*)



(*en cas de fin de jeu on augmente ou triple la profondeur de recherche, changer de fonction d'évaluation*)

(*on va essayer de doubler la profondeur de l'algominmax, sinon on va faire un algo de type A*)

(*
    On regarde la CaseObjectif, on regarde ou on est et on applique le coup pour s'en rapprocher

    Noeud de déoart dans une file de priorite-> open list
    tous les cases sont non explorées



*)
(* Calcul de la distance hexagonale entre deux cases *)




type node = {
  config: configuration;    (* Configuration actuelle *)
  path: coup list;          (* Liste des coups pour atteindre cette configuration *)
  g: int;                   (* Coût réel depuis le départ *)
  f: int;                   (* Coût total estimé (g + h) *)
}

(* Calcul de la distance hexagonale entre deux cases *)
(* Calcul de la distance hexagonale entre deux cases *)

(*
let distance_hex c1 c2 =
  let (x1, y1, z1) = c1 in
  let (x2, y2, z2) = c2 in
  (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2


(* Insérer un nœud dans la liste triée par f *)
(* Insérer un nœud dans la liste triée par f *)
let rec insert_sorted_node node open_list =
  match open_list with
  | [] -> [node]
  | hd :: tl ->
      if node.f < hd.f then
        node :: open_list
      else
        hd :: (insert_sorted_node node tl)

(* Obtenir la position actuelle du pion à déplacer *)
(* Obtenir la position actuelle du pion à déplacer *)
let get_current_position config start_case =
  try
    List.find (fun (c, _) -> c = start_case) (fst config) |> fst
  with Not_found ->
    start_case  (* Si le pion a été déplacé, retourner la position de départ *)




(* Fonction A* pour trouver une séquence de coups de start_case à goal_case *)
(* Fonction A* corrigée *)
(* Fonction A* pour trouver une séquence de coups de start_case à goal_case *)
let a_star_search config start_case goal_case =
  let initial_color = quelle_couleur start_case config in
  if initial_color = Libre || initial_color = Dehors then
    None  (* Aucun pion à déplacer *)
  else
    let initial_node = {
      config = config;
      path = [];
      g = 0;
      f = distance_hex start_case goal_case;
    } in

    let rec search open_list closed_set =
      match open_list with
      | [] -> None  (* Aucun chemin trouvé *)
      | current :: rest ->
          (* Vérifier si la case d'arrivée a été atteinte *)
          if (quelle_couleur goal_case current.config = initial_color) &&
             (quelle_couleur start_case current.config = Libre) then
            Some (List.rev current.path)  (* Chemin trouvé *)
          else if List.mem current.config closed_set then
            search rest closed_set
          else
            let closed_set' = current.config :: closed_set in
            let coups = tous_les_coups_possibles current.config in
            let enfants = List.fold_left (fun acc coup ->
              if est_coup_valide current.config coup then
                let nouvelle_config = applique_coup current.config coup in
                if not (List.mem nouvelle_config closed_set') then
                  let g_new = current.g + 1 in
                  let current_pos = get_current_position nouvelle_config start_case in
                  let h_new = distance_hex current_pos goal_case in
                  let f_new = g_new + h_new in
                  let nouveau_chemin = coup :: current.path in
                  let enfant = { config = nouvelle_config; path = nouveau_chemin; g = g_new; f = f_new } in
                  insert_sorted_node enfant acc
                else
                  acc
              else
                acc
            ) [] coups in
            search (rest @ enfants) closed_set'
    in

    search [initial_node] []

*)
let rec trouve_chemin config case_initiale case_finale profondeur_max =
  let rec aux config case_courante chemin profondeur =
    if profondeur > profondeur_max then []
    else if case_courante = case_finale then [List.rev chemin]
    else
      let coups_possibles = coup_possibles config case_courante in
      List.fold_left (fun acc (coup, nouvelle_case) ->
        let nouvelle_config = applique_coup config coup in
        let nouveaux_chemins = aux nouvelle_config nouvelle_case (coup :: chemin) (profondeur + 1) in
        acc @ nouveaux_chemins
      ) [] coups_possibles
  in
  aux config case_initiale [] 0;;

let meilleur_chemin config case_initiale case_finale profondeur_max = (*fonctionne si la case initiale est derriere la case finale*)
  let chemins = trouve_chemin config case_initiale case_finale profondeur_max in
  match chemins with
  | [] -> None
  | _ -> Some (List.hd (List.sort (fun a b -> compare (List.length a) (List.length b)) chemins));;


meilleur_chemin configuration_initial  (-4, 1, 3) (-1, 2, -1) 7;;

(* Exemple d'utilisation *)

(* Example usage *)





































































let ia_next_coup config = 
  let tete = List.hd (liste_joueurs config) in  
  let coup_possibleList = tous_les_coups_possibles config in
  match coup_possibleList with
  | [] -> failwith "aucun coup possible pour l'IA"
  | _ -> 
    let var=if est_fin_de_partie config tete 2 then 2 else 1 in 
    
    let (_, meilleur_coup) = algoMinMax1 config (3*var) min_int max_int tete (liste_joueurs config)  in
    match meilleur_coup with 
    | None -> failwith "on est dans le cas du None"
    | Some coup -> (coup, "coup effectue");;

(*on teste à la main le coup qu'il nous propose*)

configuration_initial;;

ia_next_coup configuration_initial;;

    
let testALaMain =
  ([((6, -3, -3), Vert); ((5, -2, -3), Vert); ((4, -1, -3), Vert);
  ((4, -2, -2), Vert); ((4, -3, -1), Vert); ((2, -2, 0), Vert);
  ((-6, 3, 3), Marron); ((-5, 2, 3), Marron); ((-4, 1, 3), Marron);
  ((-4, 2, 2), Marron); ((-4, 3, 1), Marron); ((-3, 2, 1), Marron)],
 [Vert; Marron]);;

 tous_les_coups_possibles testALaMain;;

gagnant testALaMain;;


 ia_next_coup testALaMain;;





















      

(*On va le tester à la main parce que ça ne fonctionne pas*)

(*on a un probème, on fait tout le temps le même coup*)




(*on fait un affichage simple pour pouvoir afficher les coups proposés*)


let string_case (case:case): string=
    let (x,y,z) =case in "("^string_of_int x^";"^string_of_int y^";"^string_of_int z^")";;


    let string_coup_aux coup =
      match coup with
      | Du (case1, case2) ->
          "Déplacement unique de la case " ^ string_case case1 ^ " à la case " ^ string_case case2 ^ ".\n"
      | Sm cases ->
          "Saut multiple à travers les cases : " ^
          (String.concat " -> " (List.map string_case cases)) ^ ".\n"
    
  

          

(*on a notre fonction d'affichage, on va tester chacun de nos coups pour voir s'ils fonctionnent, on se met en parallèle l'écran de jeu*)
let configurationDepartTest  =configuration_initial;;


(*je propose de regarder les 100 premiers coups, voir ce qu'il propose*)
(*let rec test_ia_aux config n =
  match n with
  | 0 -> ""
  | _ ->
      let (coup) = ia_next_coup config in
      let config1 = applique_coup config coup in
      let config2 = tourne_config config1 in
      let joueur_actuel = List.hd (liste_joueurs config2) in
      Printf.printf "Prochain joueur : %s\n" (string_of_couleur joueur_actuel);
      string_coup_aux coup ^ test_ia_aux config2 (n - 1);;

let test_ia config =test_ia_aux config 50;;

tourne_config configuration_initial;;

test_ia configuration_initial;;

   let _ = print_string (test_ia configuration_initial);;




   let write_to_file filename content =
    let channel = open_out filename in
    output_string channel content;
    close_out channel;;
  
  let _ = write_to_file "output.txt" (test_ia configuration_initial);;
  

(*Optimiser ? Utiliser Owl ? Réseau neuronnal ? Améliore toi*)


est_coup_valide configuration_initial (Sm([(0,2, -2); (2, 2, -4)]));;
(* Test avec la chaîne générée *)*)