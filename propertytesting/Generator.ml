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
     type 'a t = unit -> 'a;;


    (*fonction qui renvoie un generateur*)
    let next (gen : 'a t) : 'a =
      gen ()
    ;;

    (*fonction qui qui renvoie la constante constante x passée en paramètre *)
    let const x= 
      fun () -> x
    ;;

    (*cette fonction retourne générateur booléen*)
    let bool prob = 
      (*Random.float permet de choisir un float aléatoirement et 1*)
      fun () -> Random.float 1.0 < prob
      (*Si random float < prob ==> true sinon false*)
    ;;

    let int inf sup = 
      (*on verfie si inf < sup*)
      if inf <= sup then
        
        fun () -> Random.int (sup + 1 - inf) + inf
      (* permet de choisir un entier aléatoirement dans l'intervalle [inf,sup]*)
      else
        (*sinon on indique le message d'erreur avec une exception*)
        failwith "borne inf doit etre inférieur à la borne sup"
    ;;

    let int_nonneg n =
      fun() -> 
        (*si n < 0 alors on indique le message d'erreur avec une exception*)
        if n <= 0 then
          failwith "n doit être supérieur à 0"
        else
          (*sinon on choisi un entier dans l'intervalle [0,n]*)
          Random.int (n + 1)
            
    ;;

    (*cette fonction génere un float compris entre inf et sup*)
    let float inf sup = 
      (*on vérifie si inf < sup*)
      if inf <= sup then
        (*on choisi un float dans l'intervalle [inf,sup]*)
        fun () -> Random.float (sup -. inf) +. inf
      else 
        (*sinon on indique le message d'erreur avec une exception*)
        failwith "inf doit etre inferieur à sup"
    ;;

    (*cette fonction génère un float non négatif dans l'intervalle [0,n] *)
    let float_nonneg n  = 
    (*si n inférieur à 0.0*)
      if n <= 0.0 then 
        (*on lance une exception avec message d'erreur*)
        failwith "n doit être supérieur à 0"
      else
        (*sinon on choisi aléatoirement un float dans l'intervalle [0,n]*)
        fun () -> Random.float (n)
    ;;

    (*cette fonction retourne un générateur de charactère*)
    let char = 
     fun () -> 
        (*on va cherche entier aléatoire dans l'intervalle [0,52]*)
        let rand_code = Random.int (52) in
        if rand_code >25 then    (* de 26 à 51=> A->Z*)
          (*on recupere le charactère associé au code ASCII avec Char.chr*)
          Char.chr (rand_code+39)
        else                     (* de 0 à 25 => a->z*)
          Char.chr (rand_code+97)
    ;;

    (*cette fonction retourne un génateur de charactère alphanumérique*)
    let alphanum =
        fun () -> 
          (*on choisi aléatoirement un entier dans [0,62]*)
          let rand_code = Random.int (62) in
          if rand_code > 51 then    (* de 52 à 61 => chiffre*)
            Char.chr (rand_code-4)
          else if rand_code >25 then    (* de 26 à 51=> A->Z*)
            (*pour revenir dans l'intervalle [A,Z]*)
            Char.chr (rand_code+39)
          else                     (* de 0 à 25 => a->z*)
            Char.chr (rand_code+97)
    ;;


    (*cette fonction renvoie un générateur de chaine de caractère*)
    let string n gen  =
      fun () ->
        (*fontion recursive pour constuire la chaine de caractère*)
        let rec aux i acc =
          if i = n then
          (* lorsque la chaine a atteint le nombre n *)
            String.concat "" (List.rev acc)
            
          else
            (*on genere un ième carac selon la stratégie*)
            let c = gen () in
            (*et on ajoute ce caractère à la position i *)
            aux (i + 1) (String.make 1 c :: acc)
        in
        aux 0 [] (*position de départ et chaine de carac de départ*)
    ;;

     (* gen est de type unit -> 'a *)
     (*retourne un générateur de liste*)
    let list n gen = 
      fun () ->
        let rec aux i acc = 
        if i = n then
          (*Si i est égale à la taille de liste*)
          List.rev acc
        else
          (*on genere pseudo aléa un élement x selon le gen*)
          let x = gen() in
          (*et on l'ajoute à la position i*)
          aux (i + 1) (x :: acc)
          (* x est ajoutée à la liste acc *)
        in
        aux 0 [] (*position de départ et liste de départ*)
    ;;

    (*Genarator.next(Generator.list 5 (Generator.const 5));;*)

    (*retourne un générateur de couple*)
    let combine fst_gen snd_gen = 
      fun () -> (fst_gen(),snd_gen())
    ;;

    (*Generator.next(Generator.combine (Generator.int 1 10) (Generator.alphanum));;*)

    (*retourne un générateur en appliquant la fonction f*)
    let map (p : 'a -> 'b) (gen : 'a t) : 'b t =
      fun () -> p (gen ())
    ;;

    (*Generator.next(Generator.map(fun x -> x + 1) (Generator.int_nonneg 2))*)
    (*retourne une génerateur gen si celui verifie la condition p*)
    let rec filter p gen =
      (*on genere un x avec gen() *) 
      let x = gen() in
      (* on verif si x vérifie la condition p *)
      if p x then 
        fun() -> x
      else
        (*sinon on filtre le génerateur avec la condition p*)
        filter p gen
    ;;

    (*Generator.next(Generator.filter(fun x -> x mod 2 = 0)(Generator.int 7 15))*)


    (*générateur pseudo-aléatoire obtenu en appliquant `
    fst f` pour toute valeur vérifiant `p`
    et `snd f` pour toute valeur ne le vérifiant pas*)
    let partitioned_map p f gen =
      (*on genere un élement x*)
      let x = gen () in
      (*si x verifie p*)
      if p x then
        (*on applique fst f*)
        fun () -> (fst f) x
      else
        (*sinon on applique snd f*)
        fun () -> (snd f)x
    ;;
    
  end;;