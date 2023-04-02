module Property :
  sig
    (** Type d'une propriété portant sur des éléments de type 'a
      * Une propriété est une fonction booléenne. *)
    type 'a t = 'a -> bool

    (* CONSTANTES *)

    (** Propriété toujours vérifiée *)
    val always_true  : 'a t

    (** Propriété jamais   vérifiée *)
    val always_false : 'a t
  end =
  struct
    type 'a t = 
      'a -> bool
    ;;

    (* CONSTANTES *)

    let always_true = 
      (* Fonction anonyme prenant un argument ignoré "_" et renvoyant "true" *)
      fun _ -> true
    ;;

    let always_false = 
      (* Fonction anonyme prenant un argument ignoré "_" et renvoyant "false" *)
      fun _ -> false
    ;;
  end ;;