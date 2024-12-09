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


    (*q 19*)
let rec supprime_dans_config (config:configuration) (c:case) = (*à vérifier*)
  let (listeCase, listeCouleur)=config in
  match listeCase with 
  | [] ->([], listeCouleur)
  | (case, couleur)::q when case=c -> (*on le supprime*) (q, listeCouleur)
  | x::q -> let (l1, l2)=supprime_dans_config (q, listeCouleur) c in (x::l1, l2);;

(*q 20*)
let liste_joueurs (_, l) = l


    let est_coup_valide (config:configuration) (coup:coup) =
      match coup with
      | Du(c1, c2) -> 
        let couleurC2 =quelle_couleur c2 config in 
        let couleurC1= quelle_couleur c1 config in
        let couleur_joueur = List.hd (liste_joueurs config) in
        if sont_cases_voisines c1 c2 && est_dans_losange c2 dim && couleurC2=Libre && couleurC1=couleur_joueur  (*si elles sont voisines*)
            (*comment vérifier si il s'agit du pion du joueur ?*)
            then true
      else false
      | _ -> false;;

(*on passe à la question 21*)



let applique_coup config coup =
  if est_coup_valide config coup then 
    match coup with
     |  Du(c1, c2) -> let couleurC1= quelle_couleur c1 config in 
      let (listeCase, listeCouleur)=config in
      let (nouvelle_listeCase, _)=supprime_dans_config config c1 in 
        let nouvelle_listeCase=(c2, couleurC1)::nouvelle_listeCase in (nouvelle_listeCase, listeCouleur)
      | _ -> config
    else  config;;


(*on passe à la question 22*)

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
      | _ -> Error "Le coup ne contient pas deux cases ?";;
  
  

      let rec verifier_cases_aux (case : case) (vi, vj, vk) remaining_dist case_color_list : bool =
        if remaining_dist = 0 || remaining_dist = 1 then true 
        else
          let (ci, cj, ck) = case in
          if (ci < 0) then 
            if remaining_dist > 1 && List.exists (fun ((x, y, z), _) -> y = cj || z = ck) case_color_list then
              false
            else
              verifier_cases_aux (ci + vi, cj + vj, ck + vk) (vi, vj, vk) (remaining_dist - 1) case_color_list
          else 
          if remaining_dist > 1 && List.exists (fun ((x, y, z), _) -> y = cj || z = ck) case_color_list then
            false
          else
            verifier_cases_aux (ci - vi, cj - vj, ck - vk) (vi, vj, vk) (remaining_dist - 1) case_color_list
      
        ;;
      
        let est_libre_seg (c1 : case) (c2 : case) (config : configuration) : bool =
          let ((vi, vj, vk), dist) = vec_et_dist c1 c2 in
          let (case_color_list, _) = config in
          (* Supprime c1 et c2 de la liste des cases à vérifier *)
          let filtered_case_color_list =
            List.filter (fun ((x, y, z), _) -> (x, y, z) <> c1 && (x, y, z) <> c2) case_color_list
          in
          (* Appelle la fonction auxiliaire *)
          verifier_cases_aux c1 (vi, vj, vk) dist filtered_case_color_list
        ;;


(*Question 24*)
(* est_saut : 
- renvoie faux si la distance dans vec_et_dist est >= 2
- renvoie faux si la case 2 appartient à la config
- renvoie faux si la case entre case1 et case2 n'appartient pas à la config
- renvoie faux si la case2 est en dehors du losange*)

(*on suppose la dimension qu'on a pris en compte au début, peut être faire une fonction pour déterminer la dimension en fonction de la config*)
let est_saut (c1 : case) (c2 : case) (config : configuration) : bool = 
  if not(est_dans_losange c2 dim) then false 
  else 
  let (i,j,k) = c2 in
  let ( _ , d) = vec_et_dist c1 c2 in 
  if d >= 2 then false
  else 
    let (case_coloree_list, _ ) = config in
    if List.exists (fun ((x, y, z), _) ->x = i && y = j && z = k) case_coloree_list then false
    else 
      if est_libre_seg c1 c2 config then false 
      else true;;



(*on attend théo pour la suite, on passe aux restes de la partie*)

(*troisième partie*)
  

(*pour calculer le score, il suffit de sommer les (i) de chacun de ses valeurs*)

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

let directions = [ (1, -1, 0); (-1, 1, 0); (1, 0, -1); (-1, 0, 1); (0, 1, -1); (0, -1, 1) ] ;; (*il s'agit de toutes les directions dans lequel le joueur peut aller*)


(*pour les coups uniques*)
let rec listeDeplacementUnitaire_aux (config:configuration) (caseInitial:case) liste :(case * coup) list =(*on applique les vecteurs direction pour savoir ce qu'on fait*)
  match liste with 
  | [] -> []
  | t::q -> let caseSuivante=translate caseInitial t in let couleurSuiv=quelle_couleur caseSuivante config in 
    match couleurSuiv with
      | Libre ->           (caseSuivante, Du(caseInitial, caseSuivante)) :: listeDeplacementUnitaire_aux config caseInitial q
      (*il s'agit d'une case libre, il faut l'ajouter au coup possible*)
      | _ -> listeDeplacementUnitaire_aux config caseInitial q;;

      (*pour les coups multiples*)

  let listeDeplacementUnitaire (config:configuration) (caseInitial:case) :(case * coup) list =listeDeplacementUnitaire_aux (config:configuration) (caseInitial:case) directions


(*Le tri faut le faire ensuite ou maintenant ?*)

(*on cherche les erreurs dans le code*)



let rec listeDeplacementMultiple_aux (config: configuration) (caseInitial: case) (listeCasesUtilisees: case list) (listeDirection: (int * int * int) list) : (case * coup) list =
  match listeDirection with
  | [] -> [] (* Toutes les directions ont été explorées *)
  | t :: q -> (* Il reste des directions à explorer *)
      let caseActuelle =
        match listeCasesUtilisees with
        | [] -> caseInitial
        | _ -> List.hd listeCasesUtilisees (* Que se passe-t-il exactement ici ? *)
      in
      let caseSuiv = translate caseActuelle t in
      let couleurSuiv = quelle_couleur caseSuiv config in
      if couleurSuiv <> Libre && not (List.mem caseSuiv listeCasesUtilisees) then (* "<>" signifie "différent de" en OCaml *)
        let caseSuivSuiv = translate caseSuiv t in
        let couleurSuivSuiv = quelle_couleur caseSuivSuiv config in
        if couleurSuivSuiv = Libre && est_dans_etoile caseSuivSuiv && not (List.mem caseSuivSuiv listeCasesUtilisees) then (* `est_dans_etoile` vérifie si la case est dans les limites valides du plateau *)
          let nouvelleListeCasesUtilisees = caseSuivSuiv :: listeCasesUtilisees in
          let nouveauCoup =
            match listeCasesUtilisees with
            | [] -> Sm [caseInitial; caseSuivSuiv]
            | _ ->
                (* On étend le chemin du coup en ajoutant la nouvelle case *)
                let chemin =
                  match List.assoc_opt caseActuelle (List.map (fun (c, cp) -> (c, cp)) (listeDeplacementMultiple_aux config caseInitial listeCasesUtilisees directions)) with
                  | Some (Sm chemin) -> chemin
                  | _ -> [caseInitial]
                in
                Sm (chemin @ [caseSuivSuiv])
          in
          let coupsDepuisNouvelleCase = listeDeplacementMultiple_aux config caseInitial nouvelleListeCasesUtilisees directions in
          let coupsDepuisCaseActuelle = listeDeplacementMultiple_aux config caseInitial listeCasesUtilisees q in
          (caseSuivSuiv, nouveauCoup) :: (coupsDepuisNouvelleCase @ coupsDepuisCaseActuelle)
        else
          (* La case de destination n'est pas libre ou valide *)
          listeDeplacementMultiple_aux config caseInitial listeCasesUtilisees q
      else
        (* Pas de pièce à sauter ou déjà visitée, on passe à la direction suivante *)
        listeDeplacementMultiple_aux config caseInitial listeCasesUtilisees q;;

        (*on simplifie l'appelle de cette fonction*)
        let listeDeplacementMultiple (config:configuration) (caseInitial:case)= listeDeplacementMultiple_aux config caseInitial [] directions;;



(*
  let rec listeDeplacementMultiple_aux (config: configuration) (caseInitial:case)  listeCasesUtilisées listeDirection : (case*coup) list =(*il s'agit des coups en ne suivant admettant qu'on p*)
    match listeDirection with
    | [] -> [] (*on a exploré toutes les directions dans ce cas*)
    | t::q -> (*on a encore des directions à explorer*)
        match listeCasesUtilisées with 
        | _::listeCaseUtiliséesTotal -> (*la liste n'est pas vide, et il faut explorer les directions en partant de la tête de la liste*)
            let caseActuelle = List.hd listeCaseUtiliséesTotal in
            let caseSuiv=translate caseActuelle t in (*il faudrait l'appliquer 2 fois pour avoir le suivant, *)
            let couleurSuiv=quelle_couleur caseSuiv config in 
            match couleurSuiv with
            | Libre -> listeDeplacementMultiple_aux config caseInitial [] q(*dans ce cas la pas de pivot, et il faut continuer à chercher dans la suite des directions*)
            | _ -> let caseSuivSuiv =translate caseSuivS t in let couleurSuivSuiv=quelle_couleur caseSuivSuiv config in 
              match couleurSuivSuiv with
              | Libre -> (caseSuivSuiv, Du(caseInitial, caseSuivSuiv))::listeDeplacementMultiple_aux config caseInitial (caseSuivSuiv::listeCasesUtilisées) directions (*on peut appliquer notre pivot, et on ajoute listeCaseUtilisées*)(*on peut tous les reprendre donc j'appelle direction*)
              | _ -> listeDeplacementMultiple_aux config caseInitial listeCasesUtilisées q (*on ne peut rien faire, om fait retourner en arrierer et changer de pivot*)
        | [] -> let caseActuelle=caseInitial in let caseSuiv=translate caseActuelle t in (*il faudrait l'appliquer 2 fois pour avoir le suivant, *)
        let couleurSuiv=quelle_couleur caseSuiv config in 
        match couleurSuiv with
        | Libre -> listeDeplacementMultiple_aux config caseInitial [] q(*dans ce cas la pas de pivot, et il faut continuer à chercher dans la suite des directions*)
        | _ -> let caseSuivSuiv =translate caseSuiv t in let couleurSuivSuiv=quelle_couleur caseSuiv config in 
          match couleurSuivSuiv with
          | Libre -> (caseSuivSuiv, Du(caseInitial, caseSuivSuiv))::listeDeplacementMultiple_aux config caseInitial (caseSuivSuiv::listeCasesUtilisées) directions (*on peut appliquer notre pivot, et on ajoute listeCaseUtilisées*)(*on peut tous les reprendre donc j'appelle direction*)
          | _ -> listeDeplacementMultiple_aux config caseInitial listeCasesUtilisées q (*on ne peut rien faire, om fait retourner en arrierer et changer de pivot*)

        | (*lorsque c'est vide, on peut utiliser *)
*)  
  (*on fait 3 autres fonctions :
    -> saut unique
    ->plusieurs saut
    ->on trie les coups qui se répètent
    *)
(*On pourrait faire une fonction qui regarde si 1 est possible 1.1, 1.1.1, ..., 1.2, ..., une sorte d'abre*)



 (*pourquoi il peut y avoir une infinité de coup ? si on effectue différent coup toutes les cases du losange sont atteignables ?*)
(*on doit renvoyer une liste de coup possible, fonction clé*)


let coup_possible (config:configuration) (caseInitial:case) = 
  let listeCoupUnique=listeDeplacementUnitaire config caseInitial in
  let listeCoupPlusieurs=listeDeplacementMultiple config caseInitial in 
  listeCoupUnique  @ listeCoupPlusieurs ;;


coup_possible configuration_initial (0,0,0);; (*est ce que ca fais ce qui est attendu ? comment pourrais-je le vérifier ?*)
(*q 30 -Avant de faire quoique ce soit de compliqué, on va prendre une fonction qui choisit au hasard un pion
  -> il faut seulement qu'il y ait un coup possible et elle l'appliquera

  -> On regarde ensuite comment on peut choisir l'option qui permet d'augmenter le plus le score


*)



let rec case_bonne_couleur (config:configuration) = (*renvoie toutes les cases de la bonne couleur*)
  let couleur=List.hd (snd config) in 
  match fst config with
  | ([]) -> []
  | ((case, coul)::q) -> 
    if coul=couleur then case::case_bonne_couleur (q, snd config) else case_bonne_couleur(q, snd config);;


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
      let listeCoup = List.map snd (coup_possible config caseChoisie) in (* extrait les coups possibles de la case choisie *)
      let nombreHasardPourCoup = Random.int (List.length listeCoup) in 
      prendreElementAN listeCoup nombreHasardPourCoup ;;(* retourne un élément de type `coup` parmi les coups possibles *)
    
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




let rec meilleurCaseAChoisir_aux (config:configuration) (valeurMeilleurcoup:int) (meilleurCoup:coup) listesCasesPossible : coup =
  match listesCasesPossible with 
  | [] -> meilleurCoup
  | casePossibles::casesRestantes -> 
    
    let scoreInitial=score config in 
    let listeCoup =List.map snd (coup_possible config casePossibles) in

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

    let next_coup_ia2 (config:configuration) (valeurMeilleurcoup:int) (meilleurCoup:coup) listesCasesPossible : coup = (*comment on fait si aucun coup n'est disponible ? même s'il devrait toujours il y en avoir un*)
    let listesCasesPossible= case_bonne_couleur config in
    match  listesCasesPossible with 
    | [] -> failwith "aucune case n'a eté trouvé"
    | case::_ -> 
    let listecoup_defaut=coup_possible config case in (*au coup ou la liste soit nulle*)
    match listecoup_defaut with
    | [] -> failwith "la liste des coups possibles est vide pour cette couleur"
    | (case, coup)::q ->           meilleurCaseAChoisir_aux config valeurMeilleurcoup coup listesCasesPossible;;



(*comment peut-on construire un algorithme de minmax*)          

(*
          Arbre décisionnelle
            -> On évalue toutes les possibilités avec une certaine profondeur
            -> On commence à regarder par les feuilles, si ce n'est pas a moi de joueur, je regarde quel cas m'arrange le plus (on le minimise en supposant qu'il joue parfaitement)
            ->Si c'est à moi de joueur, je regarde quel cas m'arrange le plus

            ->Je choisis le cas le + favorable


            Comment faire un arbre de recherche ? Quelle profondeur (2-3 ?) ? Comment l'optimiser en passant certaines situations qui ne sont pas utiles ?
            Pourrait-on lui donner des jeux déja fait, quelle analyse le jeu et qu'elle en fasses qqch ? (réseaux neuronnales ?)

*)

(*
Fonction Minimax(etat, profondeur, joueur)
  Si profondeur = 0 ou etat est terminal
    Retourner score(etat)

  Si joueur = MAX
    MeilleurScore = -∞
    Pour chaque coup dans coups_possibles(etat)
      NouvelEtat = applique_coup(etat, coup)
      Score = Minimax(NouvelEtat, profondeur - 1, MIN)
      MeilleurScore = max(MeilleurScore, Score)
    Retourner MeilleurScore

  Sinon (joueur = MIN)
    MeilleurScore = +∞
    Pour chaque coup dans coups_possibles(etat)
      NouvelEtat = applique_coup(etat, coup)
      Score = Minimax(NouvelEtat, profondeur - 1, MAX)
      MeilleurScore = min(MeilleurScore, Score)
    Retourner MeilleurScore
*)



(*on commence à l'implémenter et ensuite, on regarde la suite des probas*)

let rec tous_les_coups_possibles_aux (config:configuration) liste = (*nous renvoie tous les coups possibles*)(*renvoie la case à laquelle on applique une liste*)
      match liste with 
      | [] -> []
      | case::listesDeCases -> coup_possible config case::tous_les_coups_possibles_aux config listesDeCases;;

let tous_les_coups_possibles_aux (config:configuration) = tous_les_coups_possibles_aux config (case_bonne_couleur config);;

let rec evaluer_coups (config:configuration) (profondeur:int) joueur_max joueur_actuel listecoup (meilleur_score, meilleur_coup) =
  match listecoup with 
  | [] -> (meilleur_score, meilleur_coup)
  | coup::reste -> 
      let nouvelle_config =applique_coup config coup in
      let algo_resul=algoMinMax nouvelle_config (profondeur - 1) joueur_max (List.hd (liste_joueurs nouvelle_config)) in
      let score=snd algo_resul in 

      let (nouveau_meilleur_score, nouveau_meilleur_coup) =
        if joueur_actuel=joueur_max then 
          if score>meilleur_score then (score, Some coup) else (meilleur_score, meilleur_coup)
      else if score<meilleur_score then (score, Some coup) else (meilleur_score, meilleur_coup)
  
and algoMinMax (config: configuration) (profondeur: int) (joueur_max: couleur) (joueur_actuel: couleur) : (int * coup option) =
  if profondeur = 0 || gagnant config then
    (score_aux config joueur_max, None)
  else
    let coups_possibles =
      List.concat (tous_les_coups_possibles_aux config)
    in
    if coups_possibles = [] then
      (score_aux config joueur_max, None)
    else
      (* Initialisation des valeurs pour maximisation/minimisation *)
      let initial_score = if joueur_actuel = joueur_max then min_int else max_int in
      evaluer_coups config profondeur joueur_max joueur_actuel coups_possibles (initial_score, None)


(* Comprendre la logique et savoir le refaire soit même

(* Fonction auxiliaire pour explorer tous les coups possibles *)
let rec evaluer_coups config profondeur joueur_max joueur_actuel coups (meilleur_score, meilleur_coup) =
  match coups with
  | [] -> (meilleur_score, meilleur_coup)
  | coup :: reste ->
      (* Applique le coup et calcule le score pour la nouvelle configuration *)
      let nouvelle_config = applique_coup config coup in
      let score, _ = algoMinMax nouvelle_config (profondeur - 1) joueur_max (List.hd (liste_joueurs nouvelle_config)) in
      (* Mise à jour du meilleur score/coup selon le joueur actuel *)


      let (nouveau_meilleur_score, nouveau_meilleur_coup) =
        if joueur_actuel = joueur_max then
          if score > meilleur_score then (score, Some coup) else (meilleur_score, meilleur_coup)
        else
          if score < meilleur_score then (score, Some coup) else (meilleur_score, meilleur_coup)
      in
      evaluer_coups config profondeur joueur_max joueur_actuel reste (nouveau_meilleur_score, nouveau_meilleur_coup)

(* Fonction principale Min-Max *)
let rec algoMinMax (config: configuration) (profondeur: int) (joueur_max: couleur) (joueur_actuel: couleur) : (int * coup option) =
  if profondeur = 0 || gagnant config then
    (score_aux config joueur_max, None)
  else
    let coups_possibles =
      List.concat (tous_les_coups_possibles_aux config)
    in
    if coups_possibles = [] then
      (score_aux config joueur_max, None)
    else
      (* Initialisation des valeurs pour maximisation/minimisation *)
      let initial_score = if joueur_actuel = joueur_max then min_int else max_int in
      evaluer_coups config profondeur joueur_max joueur_actuel coups_possibles (initial_score, None)


*)
(*Optimiser ? Utiliser Owl ? Réseau neuronnal ? Améliore toi*)

