#use "Property.ml"
#use "Generator.ml"
#use "Reduction.ml"

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
  val execute : int -> 'a t list -> ('a t * 'a option) list
end =
struct

type 'a t = {
    gen : 'a Generator.t;
    red : 'a Reduction.t;
    name : string;
    prop : 'a Property.t;
  }

  let make_test gen red name prop = { gen; red; name; prop }

  let check n test =
    let rec aux i =
      if i >= n then true
      else
        let x = Generator.next test.gen in
        let result = test.prop x in
        if result then aux (i + 1)
        else false
    in n > 0 && aux 0

  let fails_at n test =
    let rec aux i =
      if i >= n then None
      else
        let x = Generator.next test.gen in
        let result = test.prop x in
        if result then aux (i + 1)
        else
          let rec aux_red j =
            match test.red j with
            | [] -> Some x
            | ys -> match (List.find_opt (fun y -> not (test.prop y)) ys) with
                  | None -> Some j
                  | Some k when k = j -> Some j
                  | Some k -> aux_red k
          in aux_red x
    in aux 0

  let execute n tests = List.map (fun test -> (test, fails_at n test)) tests
end