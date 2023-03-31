#use "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param name nom du test
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> string -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool
    
    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list
  end =
  struct

    type 'a t = {
      gen: 'a Generator.t;
      red: 'a Reduction.t;
      name: string;
      prop: 'a Property.t;
    };;

    let make_test gen red name prop = { gen; red; name; prop }

    let check n test =
      if n <= 0 then false
      else
        let rec loop i count =
          if i >= n then count = n
          else
            let x = Generator.next(test.gen) in
            let result = test.prop x in
            if result then loop (i+1) (count+1)
            else false
        in loop 0 0;;

    (*si la propriété n'est pas vérifiée, appèle check sur les valeurs du liste retourné par test.red x. Dès que check renvoye false, retourne la valeur du liste *)
    let fails_at n test =
      let rec loop i =
        if i >= n then None
        else
          let x = Generator.next test.gen in
          let result = test.prop x in
          if not result then
            match test.red x with
            | [] -> Some x
            | ys ->
              if List.for_all test.prop ys then loop (i+1)
              else Some (List.find (fun y -> not (test.prop y)) ys)
          else loop (i+1)
      in loop 0
        

    let execute n tests =
      List.map (fun test -> (test, fails_at n test)) tests
 
  end ;;
