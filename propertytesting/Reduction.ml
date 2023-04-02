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
    
    (* LES FONCTIONS AUXILLIARES *)

    (** Convertit une chaîne de caractères en une liste de caractères
      * @param str chaîne de caractères
      * @return    liste de caractères correspondante
      *)
    val carac_par_carac : string -> char list

      (** Concatène les caractères de la liste de listes 'redMot' à la position 'i'
        * @param redMot liste de listes de caractères
        * @param i      position dans chaque liste
        * @return       chaîne de caractères concaténée
        *)
    val ligneMot : char list list -> int -> string
  
      (** Trouve la longueur maximale parmi les listes contenues dans 'tab'
        * @param tab liste de listes
        * @param i   index courant (initialiser à 0)
        * @param acc accumulateur pour la longueur maximale (initialiser à 0)
        * @return    longueur maximale parmi les listes contenues dans 'tab'
        *)
    val maxlength : 'a list list -> int -> int -> int
    
  end =
  struct
    type 'a t = 'a -> 'a list

    (* La stratégie vide : ne renvoie aucune proposition de réduction *)
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
        if x<0 then acc
        else if x=0 then aux (x::acc) (x-1)     
        else aux (x::-x::acc) (x-1)
  	  in aux [] (abs n)
    ;;


    (*retourne une liste de flottant compris entre -n et n *)
    let float n = 
      let rec aux acc x =
          if x<0. then acc
          else if x=0. then aux (x::acc) (x-. 0.5)
          else aux (x::-.x::acc) (x -. 0.5)
        in aux [] (abs_float n)
    ;;

    (*retourne une liste de flottant compris entre 0 et n*)
    let float_nonneg n = 
      let rec aux acc x = 
        if x < 0. then 
          acc
        else
          aux (x::acc) (x -. 0.5)
      in aux [] n
    ;;

    (*retourne la liste de caractère  entre 'a' et le 'carac' inclus*)
    let char c = 
      let rec aux i acc = 
      if i > Char.code c then 
        List.rev acc
      (*on verifie si le caractère courant est un carac alphanumérique*)
      else if (Char.chr i) |> (function
            | 'a' .. 'z' | 'A' .. 'Z'  -> true
            | _ -> false)
            then 
              aux (i + 1) ((Char.chr i):: acc)
      else (*sinon on passe au carac suivant*)
        aux (i+1) acc
      in aux (Char.code '0') []
    ;;
    ;;

    (* retourne la liste de caractères alphanumériques entre '0' et le 'carac' inclus *)
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
    
    (* Stratégie de réduction pour les listes *)
    (* retourne une liste de listes, où chaque élément est le résultat de 
       l'application de la fonction de réduction 'réduction' sur chaque élément de la liste d'entrée *)
    let list red lst =
      List.map red lst
    ;;

    (* Fonctions auxiliaires pour la stratégie de réduction des chaînes de caractères *)
    (* Convertit une chaîne de caractères en une liste de caractères *)
    let carac_par_carac str =
      let rec aux i acc =
        if i >= String.length str then
          List.rev acc
        else
          let c = str.[i] in
          aux (i + 1) (c :: acc)
      in aux 0 []
    ;;

    (* retourner une chaîne de caractères à partir de la liste de listes de caractères 'redMot' à l'index i *)
    let ligneMot redMot i =
      let rec aux j acc =
        if j < 0 then
          acc
        else if List.length (List.nth redMot j) > i then
          aux (j - 1) ((String.make 1 (List.nth (List.nth redMot j) i)) ^ acc)
        else
          aux (j - 1) acc
      in aux (List.length redMot - 1) ""
    ;;

    (* retourner la longueur maximale des listes dans 'tab' *)
    let rec maxlength tab i acc =
      if i >= List.length tab then
        acc
      else if List.length (List.nth tab i) > acc then
        maxlength tab (i + 1) (List.length (List.nth tab i))
      else
        maxlength tab (i + 1) acc
    

    (* Stratégie de réduction pour les chaînes de caractères *)
    (* Appliquer la fonction de réduction 'red' à chaque caractère de la chaîne de caractères 's' *)
    let string (red : char -> char list) s : string list =
      let reduction = list red (carac_par_carac s) in
      let maxL = maxlength reduction 0 0 in
      let rec aux acc i =
        if i >= maxL then
          List.rev acc
        else
          let mot = ligneMot reduction i in
          aux (mot :: acc) (i + 1)
      in aux [] 0
    ;;


    (* Combiner deux fonctions de réduction 'fst_red' et 'snd_red' pour créer une fonction de réduction sur les paires *)
    let combine fst_red snd_red (x,y): ('a * 'b) list =
      let first = fst_red x in
      let second = snd_red y in
      let rec aux acc i =
        if i < List.length first || i < List.length second then
          let a = List.nth first (i mod (List.length first)) in
          let b = List.nth second (i mod (List.length second)) in
          aux ((a,b)::acc) (i+1)
        else List.rev acc
      in aux [] 0;;

    (* Appliquer un filtre à une stratégie de réduction *)
    let filter p red= 
      fun x -> List.filter p (red x)
     ;;

  end ;;
