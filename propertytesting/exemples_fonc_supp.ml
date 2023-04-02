#use "Test.ml" ;;

(** TEST DE 
  * int_seed de Generator,
  * and_ or_ not_ implies_ de Property,
  * fails_at_init de Test
  *)

(* Define some properties *)
let is_positive x = x > 0 ;;
let is_even x = x mod 2 = 0 ;;
let is_divisible_by_3 x = x mod 3 = 0 ;;

(* Construct compound properties using and_, or_, not_, and implies_ *)
let p1 = Property.and_ is_positive is_even ;;
let p2 = Property.or_ is_positive is_divisible_by_3 ;;
let p3 = Property.not_ is_positive ;;                       
let p4 = Property.implies_ is_positive is_divisible_by_3 ;;

(* TEST : Tests de la propriété p1 *)
let test_p1 = 
  let gen_p1_input = Generator.filter is_positive (Generator.int_seed (-10) 10 123)
  in let red_p1_input = Reduction.int
  in let prop_p1 = p1
  in Test.make_test gen_p1_input red_p1_input "is_positive && is_even" prop_p1
;;
(* TEST : Tests de la propriété p2 *)
let test_p2 = 
  let gen_p2_input = Generator.int_seed (-10) 10 123
  in let red_p2_input = Reduction.int
  in let prop_p2 = p2
  in Test.make_test gen_p2_input red_p2_input "is_positive || is_divisible_by_3" prop_p2
;;
(* TEST : Tests de la propriété p3 *)
let test_p3 = 
  let gen_p3_input = Generator.filter (Property.not_ is_positive) (Generator.int_seed (-10) 10 123)
  in let red_p3_input = Reduction.int
  in let prop_p3 = p3
  in Test.make_test gen_p3_input red_p3_input "not is_positive" prop_p3
;;
(* TEST : Tests de la propriété p4 *)
let test_p4 = 
  let gen_p4_input = Generator.filter is_positive (Generator.int_seed (-10) 10 123)
  in let red_p4_input = Reduction.int
  in let prop_p4 = p4
  in Test.make_test gen_p4_input red_p4_input "is_positive => is_divisible_by_3" prop_p4
;;
Test.fails_at_init 100 test_p1 ;;
Test.fails_at_init 100 test_p2 ;;
Test.fails_at_init 100 test_p3 ;;
Test.fails_at_init 100 test_p4 ;;