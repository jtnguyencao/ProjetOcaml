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
    type 'a t = 'a -> 'a list ;;

    (* TODO : Implémenter tous les éléments de la signature manquants *)

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

    (* cette fonction renvoie une décomposition de la chaine carac par carac *)
    (*
    let string str = 
      let rec aux i acc =
        if i >= String.length str then
          List.rev acc
        else
          let c = String.sub str i 1 in
          aux (i + 1) (c :: acc)
      in aux 0 []
    ;;

    (*renvoie une sous liste de la chaine de carac *)

    let list str =
      let rec aux stri acc =
        if stri >= String.length str then
          List.rev acc
        else
          let c = String.sub str 0 (stri + 1) in
          aux (stri + 1) (c :: acc)
      in aux 0 []
    ;;
    *)
  end ;;
