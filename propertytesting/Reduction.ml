module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant positif
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t

    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)
    
    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t
    
    (* LISTES *)
    
    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t
    
    (* TRANSFORMATIONS *)
    
    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t
    
    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
    
  end =
  struct
    type 'a t = 'a -> 'a list

    (* TODO : Implémenter tous les éléments de la signature manquants *)
    
    let empty x = 
      []
    ;;

    (*retourne une liste compris entre 0 et n*)
    let int_nonneg n  = 
      let rec aux acc x = 
        if x < 0 then 
          acc
        else
          aux (x::acc) (x-1)
      
      in aux [] n  
    ;;

    (*retourne une liste d'entiers compris entre -n et n*)
    let int n = 
      let rec aux acc x =
        if abs x > n then acc
        else aux (x::acc) (x+1)
  	  in aux [] (-n)
    ;;


    (*retourne une liste de flottant compris entre -n et n *)
    let float n = 
      let rec aux acc x =
          if x > n then acc
          else aux (x::acc) (x +. 0.5)
        in aux [] (-.n)
    ;;

    (*retourne une liste de flottant compris entre 0 et n*)
    let float_nonneg n = 
      let rec aux acc x = 
        if x > n then 
          acc
        else
          aux (x::acc) (x +. 0.5)
      in aux [] 0.0
    ;;

    (*retourne la liste de caractère  entre 'a' et le 'carac' inclus*)
    let char c = 
      let rec aux i acc = 
      (*on regarde si le char i est sup au code ASCII*)
        if i > Char.code c then
          List.rev acc
        else 
          aux (i + 1) (Char.chr i:: acc)
      in aux (Char.code 'a') []
    ;;


    let alphanum c = 
      let rec aux i acc = 
      if i > Char.code c then 
        List.rev acc
      (*on verifie si le caractère courant est un carac alphanumérique*)
      else if (Char.chr i) |> (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
            | _ -> false)
            then 
              aux (i + 1) ((Char.chr i):: acc)
      else (*sinon on passe au carac suivant*)
        aux (i+1) acc
      in aux (Char.code '0') []
    ;;

    (*Pour tester : alphanum '9';;*)
    
    
    (* Cette fonction renvoie une liste de chaînes de caractères plus "simples" au pire aussi longues que `s` *)
    let string (red : char -> char list) s : string list =
      let rec aux i acc =
        if i >= String.length s then
          List.rev acc
        else
          let c = s.[i] in
          let cs = red c in
          let strs = List.map (String.make 1) cs in
          aux (i + 1) (strs @ acc)
      in aux 0 []
    ;;

    
    let list red lst =
      let rec aux i acc =
        if i >= List.length lst then
          List.rev acc
        else
          (* pour acceder au i-ième element*)
          let c = List.nth lst i in 
          (*on applique la reduction sur le i ème element*)
          let c' = red c in
          (*on l'ajoute à liste accumulée acc *)
          aux (i + 1) (c' :: acc)
      in aux 0 []
    ;;

    let combine fst_red snd_red : ('a * 'b) t = 
      fun(x,y) ->
        let first = fst_red x in
        let second = snd_red y in
        List.combine first second
    ;;


    (*IMPORTANT*)
    (*CELLE LA EST A REFAIRE, il y a un soucis avec le typage mais je vois pas comment résoudre*)


    let filter p red : 'a t = 
      (*la fonction aux permet d'ajouter le i eme élement à la liste de propositions si
      i il verifie la condition p*)
      let rec aux p red i acc =
        if i >= List.length acc then
          (**)
          List.rev acc
        else
          (*List.nth list indice : pour accéder au ième element*)
          let x = List.nth acc i in
          (*si le ième element verifient la condition p*)
          if p x then
            
            let add = red x in (* on l'ajoute à la liste accumulée acc*)
            (* et on regarder le i+1 element*)
            aux p red (i+1) (add :: acc)
          else
            (*sinon on passe direct au i+1 element*)
            aux p red (i+1) acc
        
      in
      aux p red 0 [] (*liste de depat et indice de depart*)
    ;;

    (*)
    
    
    
  end ;;
