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
    type 'a t = 'a -> bool ;;

    (* CONSTANTES *)

    let always_true : 'a t = fun _ -> true

    let always_false : 'a t = fun _ -> false
  end ;;
