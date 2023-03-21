module Generator :
  sig
    (** Type du générateur pseudo-aléatoire de données de type 'a *)
    type 'a t

    (** Renvoie une nouvelle valeur aléeatoire
      * @param gen générateur pseudo-aléatoire
      * @return    nouvelle valeur aléatoire en utilisant `gen`
      *)
    val next : 'a t -> 'a
   
    (** Générateur constant
      * @param x valeur
      * @return  générateur de l'unique valeur `x`
      *)
    val const : 'a -> 'a t

    (* GENERATEURS DE TYPES DE BASE *)
 
    (** Générateur pseudo-aléatoire de booléens
      * @param prob probabilité de la valeur `true`
      * @return     générateur pseudo-aléatoire de valeurs booléennes
      *)
    val bool : float -> bool t
  
    (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
      *)
    val int : int -> int -> int   t

    (** Générateur pseudo-aléatoire d'entiers positifs ou nuls
      * @param n borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre 0 et `n` inclus
      *)
    val int_nonneg : int -> int   t

    (** Générateur pseudo-aléatoire de flottants
      * @param x borne supérieure
      * @param y borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre `x` et `y` inclus
      *)
    val float : float -> float -> float t

    (** Générateur pseudo-aléatoire de flottants positifs ou nuls
      * @param x borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre 0 et `x` inclus
      *)
    val float_nonneg : float -> float t

    (** Générateur pseudo-aléatoire de caractères *)
    val char : char t

    (** Générateur pseudo-aléatoire de caractères alphanumériques *)
    val alphanum : char t

    (* GENERATEURS DE CHAINE DE CARACTERE *)

    (** Générateur de chaînes de caractères
      * @param n   longueur maximale de la chaîne de caractère
      * @param gen générateur pseudo-aléatoire de caractères
      * @return    générateur pseudo-aléatoire de chaînes de caractères dont chaque caractéré est généré avec `gen`
      *)
    val string : int -> char t -> string t

    (* GENERATEURS DE LISTES *)

    (** Générateur de listes
      * @param n   longueur maximale de la liste
      * @param gen générateur pseudo-aléatoire d'éléments
      * @return    générateur pseudo-aléatoire de listes dont chaque élément est généré avec `gen`
      *)
    val list : int -> 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Générateur pseudo-aléatoire de couples
      * @param fst_gen générateur pseudo-aléatoire de la première coordonnée
      * @param snd_gen générateur pseudo-aléatoire de la deuxième coordonnée
      * @return        générateur pseudo-aléatoire du couple
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un post-traitement à un générateur pseudo-aléatoire
      * @param f   post-traitement à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `f` à chaque valeur générée par `gen`
      *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Applique un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire ne générant des valeurs de `gen` que si elles vérifient `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** Applique un post-traitement dépendant d'un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param f   couple des post-traitements à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `fst f` pour toute valeur vérifiant `p`
      *                                                          et `snd f` pour toute valeur ne le vérifiant pas
      *)
    val partitioned_map : ('a -> bool) -> (('a -> 'b) * ('a -> 'b)) -> 'a t -> 'b t
    
  end =
  struct
    (* TODO : Implémenter le type et tous les éléments de la signature *)
     type 'a t = unit -> 'a;;

    let next (gen : 'a t) : 'a =
      gen ()
    ;;

    let const x= 
      fun () -> x
    ;;

    let bool prob = 
      fun () -> Random.float 1.0 < prob
    ;;

    let int inf sup = 
      fun () ->
        if inf <= sup then
          Random.int (sup + 1 - inf) + inf
        else
          failwith "borne inf doit etre inférieur à la borne supe"
    ;;

    let int_nonneg n =
      fun() -> 
        if n <= 0 then
          failwith "n doit être supérieur à 0"
        else
             
          Random.int (n + 1)
            
    ;;

    (*cette fonction génere un float compris entre inf et sup*)
    let float inf sup = 
      fun () -> 
        if inf <= sup then
          Random.float (sup -. inf) +. inf
        else 
          failwith "inf doit etre inferieur à sup"
    ;;

    let float_nonneg n  =
    fun () ->
        if n <= 0.0 then 
          failwith "n doit être supérieur à 0"
        else
          Random.float (n)
    ;;

    let char = 
      fun () -> 
      let rand_code = Random.int(Char.code 'z' - Char.code 'a' + 1) in
      Char.chr(Char.code 'a' + rand_code)
    ;;

    let alphanum =
      fun () -> 
        let rand_code = Random.int (62) in
        if rand_code > 51 then    (* de 52 à 61 => chiffre*)
          Char.chr (rand_code-4)
        else if rand_code >25 then    (* de 26 à 51=> A->Z*)
          Char.chr (rand_code+39)
        else                     (* de 0 à 25 => a->z*)
          Char.chr (rand_code+97)
    ;;

    let string n gen  =
      fun () ->
        let rec aux i acc =
          if i = n then
          (* lorsque la chaine a atteint le nombre n *)
            String.concat "" acc
            
          else
            (*on genere un carac selon la stratégie*)
            let c = gen () in
            aux (i + 1) (String.make 1 c :: acc)
        in
        aux 0 []
    ;;

     (* gen est de type unit -> 'a *)
    let list n gen = 
      fun () ->
        let rec aux i acc = 
        if i = n then
          acc
        else
          (*on genere pseudo aléa un élement x *)
          let x = gen() in
          aux (i + 1) (x :: acc)
          (* x est ajoutée à la liste acc *)
        in
        aux 0 []
    ;;

    (*Genarator.next(Generator.list 5 (Generator.const 5));;*)

    let combine fst_gen snd_gen = 
      fun () -> (fst_gen(),snd_gen())
    ;;

    let map (p : 'a -> 'b) (gen : 'a t) : 'b t =
      fun () -> p (gen ())
    ;;

    let filter p gen =
      fun() ->
        let rec tmp p gen =
          (*on genere un x avec gen() *) 
          let x = gen() in
          (* on verif si x vérifie la condition p *)
          if p x then 
            x
          else
            tmp p gen
          in
          tmp p gen
    ;;

    let partitioned_map p f gen =
      fun() ->
        let x = gen () in
        if p x then
          (fst f) x
        else
          (snd f)x
    ;;



        
  end