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

    (*génarateur constant*)

    let const x = 
      x
    ;;
    
    
    (* générateur de pseudo aléa de bool *)
    
    let bool prob = 
      if Random.float 1.0 < prob then
        true
      else
        false	
    ;;

    (* générateur pseudo al d'un entier compris entre inf et sup *)
    let int inf sup = 
      let integer = Random.int (sup -inf) in
      if integer <= sup && integer >= inf then
        integer
      else (*sinon je retourne la borne sup*)
        sup
    ;;

    (* Cette fonction renvoie un générateur pseudo-aléatoire d'entiers naturels >= 0 *)
    let int_nonneg n =
      if n <= 0 then
        failwith "n doit être supérieur à 0"
      else
          let integer = Random.int n in
          if integer < n then integer
          else 0   
    ;;

    (*cette fonction génere un float compris entre inf et sup*)
    let float inf sup = 
      let integer = Random.float (sup -. inf) in
      if integer <= sup && integer >= inf then
        integer
      else
        sup
    ;;


    (*cette fonction génere un float compris entre 0 et n*)
    let float_nonneg n = 
      if n <= 0 then 
        failwith "n doit être supérieur à 0"
      else
        let myfloat = Random.float n in 
        if myfloat < n then myfloat
        else 0.0
    ;;

    (*cette fonction génere un char compris entre inf et sup*)
    let char () = 
      let rand_code = Random.int (Char.code 'z' - Char.code 'a' + 1) in
      Char.chr (Char.code 'a' + rand_code)
    ;;
    
    
   
    (*cette fonction génere un char alpha numériques *)
    let alphanum () =
      let rand_code = Random.int (Char.code 'z' - Char.code '0' + 1) in
      let char_code = Char.code '0' + rand_code in
      if char_code > Char.code '9' then
        Char.chr (Char.code 'a' + rand_code - (Char.code '9' - Char.code '0') - 1)
      else
        Char.chr char_code
    ;;

    let string n (gen : unit -> char) =
      let rec aux i acc =
        if i = n then
        (* lorsque la chaine a atteint le nombre n *)
          String.concat "" (List.rev acc)
          
        else
          (*on genere un carac selon la stratégie*)
          let c = gen () in
          aux (i + 1) (String.make 1 c :: acc)
      in
      aux 0 []
    ;;

    (* gen est de type unit -> 'a *)
    let list n gen = 
      let rec aux i acc = 
      if i = n then
        List.rev acc
      else
        (*on genere pseudo aléa un élement x *)
        let x = gen() in
        aux (i + 1) (x :: acc)
        (* x est ajoutée à la liste acc *)
      in
      aux 0 []
    ;;


    let combine fst_gen snd_gen = 
      fun () -> (fst_gen(),snd_gen())
    ;;
    (*
    pour tester:
      
    Dans le terminal : 

    let gen = combine (fun () -> string 5 alphanum) (fun () -> int 1 10)
    let result = gen ()

    *)

    (*Pour map, je ne sais pas si ça marche correctment*)
    (*Terminal : map(fun x -> x mod 2 = 0)(fun () -> int 5 10)*)

    let map p gen =
      let rec aux () =
        let x = gen () in
        if p x then x
        else aux ()
      in
      aux
    ;;

    let rec filter p gen () =
      (*on genere un x avec gen() *) 
      let x = gen() in
      (* on verif si x vérifie la condition p *)
      if p x then 
        x
      else
        filter p gen ()
    ;;
    
    (*code pour tester la fonction filter *)
    
    let even x = x mod 2 = 0;;
    
    let gen = int_nonneg 100;;
    
    let gen_even = filter even (fun() ->gen);;
    
    let rec print_values n =
      if n = 0 then
        ()
      else
        let x = gen_even () in
        print_int x;
        print_string " ";
        print_values (n-1)
    ;;
    
    let () =
      print_values 10
    ;;


  end ;;
