open Ast
open Eval

(** ============================================================================*)
(** Testing                                                                     *)
(** *===========================================================================*)

(** ===== Testing pre and post processing functions =========================== *)

let example1 =
    Par(
      Par(Send("x", "y"),
          Recv("y", "z",
               New("z",
                   New("z",
                       Par(
                         Recv("x", "y", Zero),
                         Send("y", "z")
                       )
                      )
                  )
              )
         ),
      New("y",
          Par(Recv ("x", "y", Zero), Send("y", "z"))
         )
    )
let example2 = Par(Send("x", "y"), Recv("y", "z", Par(Send("w", "z"), Recv("z", "v", Zero))))
let example3 = Par(Send("x", "y"), Recv("y", "y", Par(Send("w", "y"), Recv("y", "y", New("y~", Send("y~", "y~"))))))

let _ =
  let example1_precomputed = precomputation example1 in
  let example1_postcomputed = List.hd (List.hd (postcomputation [[example1]])) in

  print_endline ("orig: " ^ to_string example1) ; 
  print_endline ("pre: " ^ to_string example1_precomputed) ; 
  print_endline ("post: " ^ to_string example1_postcomputed) ;

(** *Testing subst function ======================== *)

  let sub_example2 = Par(Send("w", "z"), Recv("z", "w", Zero)) in
  let sub_example2_subst = subst sub_example2 "z" "x" in
  print_endline("subst1: " ^ to_string sub_example2_subst) ;

  let example2_subst = subst example2 "y" "k" in
  print_endline("orig: " ^ to_string example2) ;
  print_endline("subst2: " ^ to_string example2_subst);

  let example3_subst = subst example3 "y" "k" in
  print_endline("orig: " ^ to_string example3) ;
  print_endline("subst3: " ^ to_string example3_subst) ;
()

(** ====== Testing message passing evaluation ================================= *)

let e0 = Par (Zero, Zero)
let e1 = (Par (Send ("x", "hi" ), Recv ("x", "y", Zero)))
let e2 = (Par (Send ("x", "hi" ), Par (Send ("x", "bye"), Recv ("x", "y", Zero))))
let e3 = (Par (Send ("x", "hi" ), BangR ("x", "y", Zero)))
let e4 = (Par (Par (BangR ("y", "k", Zero), Send ("x", "hi" )),
  Par (Send ("x", "bye"), Par (BangR ("x", "y", Zero), Send ("y", "Hi")))))
let e5 = (Par (Send ("x", "y"), Recv ("x", "y", Send ("x", "y"))))

let test_suite = [e0; e1; e2; e3; e4; e5]
let test_suite_2 = [example1; example2; example3]
let test (fn : exp -> exp list list) (e : exp list) =
  let test_eval (fn : exp -> exp list list) (e : exp) =
    Printf.printf "Original expression: \n \t %s \n" (to_string e);
    Printf.printf "Reduced expressions: \n \t";
    fn e |> pretty_print;
    Printf.printf "\n" in
  List.iter (test_eval fn) e

let _ =
        Printf.printf "========== Eval Testing ===========\n";
        test pi_eval test_suite_2;
        Printf.printf "========== Fuel Testing ===========\n";
        test (pi_fuel 3) test_suite;
()
