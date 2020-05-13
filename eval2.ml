open Util
open Printf

(** *===========================================================================*)
(**                                                                             *)
(**                        π - CALCULUS EVALUATOR                               *)
(**                                                                             *)
(** *===========================================================================*)

(* * General idea:

   Use flatten to go through the AST to pull out all top level sends / receives.
   Iterate this list to build a mapping of receivers and senders (keyed by
   channel name). Find every pairing of senders and receivers communicating on
   the same channel. For each such pairing, apply the reduction rule to that
   communication to obtain a new AST. Recurse on that AST.

   * Technicalities:

   To deal with variable bindings, add a preprocessing step
   where you iterate the AST and find all occurences of new. If you find a new
   operator in the form of (new x).P in the AST, iterate over P and replace all
   occurences of x with a new unique identifier.
*)

type id = string


(** *===========================================================================*)
(** Language : Core Pict AST                                                    *)
(** *===========================================================================*)
type exp =
  | Send of id * id
  | Recv of id * id * exp
  | BangR of id * id * exp
  | Par of exp * exp
  | New of id * exp
  | Zero

(** *===========================================================================*)
(** Utility Functions                                                           *)
(** *===========================================================================*)
let rec to_string (e : exp) =
  match e with
  | Send (c, v) -> sprintf "%s!%s" c v
  | Recv (c, v, p) -> sprintf "%s?%s = (%s)" c v (to_string p)
  | BangR (c, v, p) -> sprintf "%s?*%s = (%s)" c v (to_string p)
  | Par (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | New (v, p) -> sprintf "new %s (%s)" v (to_string p)
  | Zero -> sprintf "0"

(* Communications on channels is dealt with by keeping track of a map. *)
module StringMap = Map.Make(String)

open StringMap

(* Remove duplicates in a list using HashSet. *)
let remove_duplicates (l : 'a list) : 'a list =
  let h = HashSet.make () in
  List.iter (fun x -> HashSet.add h x) l;
  h |> HashSet.values

(* Permutations from https://gist.github.com/Bamco/6151962 *)
(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst =
  match lst with
  | [] -> [[x]]
  | hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl))

let rec permutations lst =
  match lst with
  | hd::tl -> List.concat (List.map (interleave hd) (permutations tl))
  | _ -> [lst]


let rec take n list =
  if n > 0 then
    match list with
    | [] -> failwith "Not enough elements in list"
    | x :: xs -> x :: (take (n - 1) xs)
  else []

let rec take_last n list =
  if n = 0 then list
  else
    match list with
    | [] -> []
    | x :: xs -> (take_last (n - 1) xs)

(** *===========================================================================*)
(** Variable Substitution Utility Functions                                     *)
(** *===========================================================================*)

(* Get all free variables of an expression. *)
let fv (e : exp) : id HashSet.t =
  let h = HashSet.make () in
  let rec fv (bv : id list) (e : exp) : unit =
    match e with
    | Send (c, v) ->
      if List.mem c bv then () else HashSet.add h c;
      if List.mem v bv then () else HashSet.add h v;
    | Recv (c, v, p) | BangR (c, v, p) ->
      if List.mem c bv then () else HashSet.add h c;
      fv (v :: bv) p
    | Par (p, p') ->
      fv bv p; fv bv p'
    | New (v, p) ->
      fv (v :: bv) p
    | Zero -> () in
  fv [] e; h

(* Get all variables of an expression. *)
let allv (e : exp) : id HashSet.t =
  let h = HashSet.make () in
  let rec allv (e : exp) : unit =
    match e with
    | Send (c, v) -> HashSet.add h c; HashSet.add h v
    | Recv (c, v, p) | BangR (c, v, p) ->
      HashSet.add h c; HashSet.add h v; allv p
    | Par (p, p') -> allv p; allv p'
    | New (v, p) -> HashSet.add h v; allv p
    | Zero -> () in
  allv e; h

(* Substitute replacee by replacer in expression e, avoiding capture. *)
let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
  let allvars = allv e in
  HashSet.add allvars replacer ;
  let fresh = Fresh.make (allvars) in
  let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
    match e with
    | Send (id1, id2) ->
      let new_id1 = if id1 = replacee then replacer else id1 in
      let new_id2 = if id2 = replacee then replacer else id2 in
      Send(new_id1, new_id2)
    | Recv (id1, id2, e) ->
      let new_id1 = if id1 = replacee then replacer else id1 in
      (* If id2 is the same as the variable being substituted out, generate a
         new name and substitute all occurrences of id2 in the subexpression
         with this new name *)
      let new_id2 = if id2 = replacee then Fresh.next fresh else id2 in
      let new_e = if id2 = replacee then (subst e id2 new_id2) else (subst e replacee replacer) in
      Recv(new_id1, new_id2, new_e)
    | BangR (id1, id2, e) ->
      let new_id1 = if id1 = replacee then replacer else id1 in
      let new_id2 = if id2 = replacee then Fresh.next fresh else id2 in
      let new_e = if id2 = replacee then (subst e id2 new_id2) else (subst e replacee replacer) in
      BangR(new_id1, new_id2, new_e)
    | Par (e1, e2) ->
      let new_e1 : exp = subst e1 replacee replacer in
      let new_e2 : exp = subst e2 replacee replacer in
      Par (new_e1, new_e2)
    | New (id, e) ->
      let new_id = if id = replacee then Fresh.next fresh else id in
      let new_e = if id = replacee then (subst e id new_id) else (subst e replacee replacer) in
      New (new_id, new_e)
    | Zero -> Zero in
  subst e replacee replacer

(** *===========================================================================*)
(** Pre- and Post- Computation                                                  *)
(** *===========================================================================*)
(** The Pre- and Post- Computation gets around stating the scope extrusion principle in
  * pi-calculus. *)

(* Naive version of subst that is only used for the pre and post computation steps *)
let rec naive_subst (e : exp) (replacee : id) (replacer : id) : exp =
  match e with
  | Send (id1, id2) ->
    let new_id1 = if id1 = replacee then replacer else id1 in
    let new_id2 = if id2 = replacee then replacer else id2 in
    Send(new_id1, new_id2)
  | Recv (id1, id2, e) ->
    let new_id1 = if id1 = replacee then replacer else id1 in
    let new_id2 = if id2 = replacee then replacer else id2 in
    let new_e = naive_subst e replacee replacer in
    Recv (new_id1, new_id2, new_e)
  | BangR (id1, id2, e)->
    let new_id1 = if id1 = replacee then replacer else id1 in
    let new_id2 = if id2 = replacee then replacer else id2 in
    let new_e = naive_subst e replacee replacer in
    BangR (new_id1, new_id2, new_e)
  | Par (e1, e2) ->
    let new_e1 : exp = naive_subst e1 replacee replacer in
    let new_e2 : exp = naive_subst e2 replacee replacer in
    Par (new_e1, new_e2)
  | New (id, e) ->
    let new_id = if id = replacee then replacer else id in
    let new_e = naive_subst e replacee replacer in
    New (new_id, new_e)
  | Zero -> Zero

let clean_name (id: id) : id =
  List.hd(String.split_on_char '~' id)

let rec precomputation (e: exp) : exp =

  let make_unique (r: int ref) (id: id) : id =
    let id = clean_name id in
    let suffix = String.init !r (fun _ -> '~') in
    r := !r + 1 ;
    id ^ suffix in

  let r : int ref = ref 1 in
  let rec precomputation (e: exp) (r: int ref) : exp =
    match e with
    | New (id, e) ->
      let new_id = make_unique r id in
      let renamed_e = naive_subst e id new_id in
      let new_e = precomputation renamed_e r in
      New (new_id, new_e)
    | Recv (id1, id2, e) -> Recv (id1, id2, precomputation e r)
    | BangR (id1, id2, e) -> BangR (id1, id2, precomputation e r)
    | Par (e1, e2) ->
      let new_e1 = precomputation e1 r in
      let new_e2 = precomputation e2 r in
      Par(new_e1, new_e2)
    | _ -> e in
  precomputation e r

let rec postcomputation (ell : exp list list) : exp list list =
  let rec clean_names (e : exp) : exp =
    let names_list = HashSet.values (allv e) in
    List.fold_right (fun id clean_e -> naive_subst clean_e id (clean_name id)) names_list e in
  List.map (fun el -> List.map clean_names el) ell

(** *===========================================================================*)
(** Message-passing Evaluation                                                  *)
(** *===========================================================================*)

(* ===== Keeping track of channels ============================================ *)

(* The returned channels is a list of channels, indexed by the channel string
 * with values that keep track of the "sent list" and "receive list" over a
 * channel.
 *
 * The input list [l : exp list] is the flattened "top-level" parallel
 * processes, and we only care about the Send and Recv events on this level of
 * exposure, so the function does not take a recursive call to its subexpressions.
 *
 * For instance, given the process
 * [Send ("x", "Achoo!"); Recv ("x", "y", e))], the resulting map will have
 * ("x", ([0], [1])).
 *)
let map_channels (l : (int * exp) list) =
  let map_channels m (ie : int * exp) =
    match ie with
    | (i, Recv (id1, id2, e')) ->
        let update_recv x = match x with
                            | None -> Some ([],[i])
                            | Some (sl, rl) -> Some (sl, i :: rl) in
        update id1 update_recv m
    | (i, Send (id1, id2)) ->
        let update_send x = match x with
                            | None -> Some ([i],[])
                            | Some (sl, rl) -> Some (i :: sl, rl) in
        update id1 update_send m
    | _ -> m in
  List.fold_left map_channels empty l

(* Given two lists, get all possible matchings of the elements in the lists. *)
let rec matchings (l1 : 'a list) (l2 : 'b list) =
  let min_length = min (List.length l1) (List.length l2) in
  let (perms, l') =
    if (List.compare_lengths l1 l2) < 0
      then (permutations l2, l1) else (permutations l1, l2) in
  let trimmed_perms = List.map (fun x -> take (List.length l') x) perms in
  List.map (fun x -> List.combine x l') trimmed_perms

(* Given a pair of expressions that are message passing, perform the message pass. *)
let message_pass (comm_pair : exp * exp) : exp * exp =
  match comm_pair with
  | (Send (c, v), Recv (c', x, e')) | (Recv (c', x, e'), Send (c, v)) ->
      (Zero, subst e' x v)
  | _ -> failwith "Non-message passing communication"

let swap_message_pass (l : exp list) (i1 : int) (i2 : int) : exp list =
  let (e1, e2) = message_pass (List.nth l i1, List.nth l i2) in
  List.mapi (fun i x -> if i = i1 then e1 else if i = i2 then e2 else x) l

(* To deal with bang do the following in flatten. Whenever you encounter a bang,
  * pull out a copy of the underlying receive and add it to the list that flatten
  * is building to be used in evaluate. When reducing, keep the bang in the AST. *)

let rec flatten m (e : exp) =
  match e with
  | Par (e1, e2) ->
    let (m, l) = flatten m e1 in
    let (m', l') = flatten m e2 in
    (m', l @ l')
  | New (v, e1) ->
    let (m, l) = flatten m e1 in
    (add v (List.length l) m, l)
  | e -> (m, [e])

let unfold_bang (e : exp list) : exp list=
    let unfold_bang (e : exp) =
    match e with
    | BangR (id1, id2, e') -> Recv (id1, id2, e') :: [e]
    | e -> [e] in
  List.map unfold_bang e |> List.flatten

(* ===== Evaluation of processed expression ================================== *)

(* This function takes care of a single "evaluation" of a pi-calc expression,
 * assuming that it has already been processed (i.e. no "New" operators) *)
let rec eval_flattened (e : exp list) : exp list list =
  (* Every evaluation step does a lazy unfolding of bang. *)
  let exp_list = unfold_bang e in

  let flattened_list_indexed = List.mapi (fun i x -> (i, x)) exp_list in
  (* Get the mapping of communications along channels *)
  let channels = map_channels flattened_list_indexed in

  (* All possible communications along flattened list, indexed by channels *)
  let channel_communications = map (fun (s, r) -> matchings s r) channels
    |> bindings in

  (* Retreiving all possible pairs of communications, not regarding channels *)
  let pairs_communications = List.map (fun (a, b) -> b) channel_communications
    |> List.flatten |> List.flatten in

  (* Resulting evaluation swaps out the possible message passing communications with
   * original indexed list. *)
  let result = List.map (fun (i1, i2) -> swap_message_pass exp_list i1 i2)
    pairs_communications in

  (* If there were no communications, return flattened list with bang unfolding. *)
  if List.length result = 0 then [exp_list] else result |> remove_duplicates


(* let clean_name (id: id) : id =
 *   List.hd(String.split_on_char '~' id) *)

(* let rec postcomputation (ell : exp list list) : exp list list =
 *   let rec clean_names (e : exp) : exp =
 *     let names_list = HashSet.values (allv e) in
 *     List.fold_right (fun id clean_e -> naive_subst clean_e id (clean_name id)) names_list e in
 *   List.map (fun el -> List.map clean_names el) ell *)

let list_to_par (e : exp list) : exp =
  List.fold_left (fun x y -> Par (x, y)) Zero e

let eval (e : exp) : exp list list =
  let rec par_flatten (e : exp) : exp list =
    match e with
    | Par (e1, e2) -> par_flatten e1 @ par_flatten e2
    | e -> [e] in
  let par_flat = par_flatten e in
  let no_new = e |> par_flatten |> unfold_bang in
  let (m, l) = flatten empty e in
  let rec rebind (e : exp list) =
    match e with
    | [] -> []
    | h :: t -> let vars = HashSet.values (allv h) in
       if (List.exists (fun s -> String.contains s '~') vars) then
       let v_bound = List.find (fun s -> String.contains s '~') vars in
       match find_first_opt (fun x -> String.contains x '~' && List.mem x vars) m with
       | Some (x, n) -> let a = (h :: (take (n - 2) t) |> list_to_par) in
          New (x, a) :: (rebind (take_last ((List.length t) - n - 1) t))
       | None -> h :: (rebind t)
       else h :: (rebind t) in
  let result = eval_flattened l |> List.map rebind in
  result

(* ===== Fuel-based evaluation =============================================== *)
(* Since some pi-calc expressions may never terminate, each step of evaluation is
 * a form of lazy evaluation. (The bang is always left as a thunk.) As an approximating
 * evaluation to diverging compuation, we provide a fuel-based evaluation. *)
let fuel (x : int) (e : exp): exp list list =
  let rec fuel_eval (e : exp list) (fuel : int) : exp list list =
    if fuel = 0 then eval_flattened e else
      let step : exp list list = eval_flattened e in
      let result = List.map (fun (x : exp list) -> fuel_eval x (fuel - 1)) step |> List.flatten in
      result |> remove_duplicates
 in
  let (m, l) = flatten empty e in
  fuel_eval l x

(** ============================================================================*)
(** Top-level Evaluator, with Pre-and Post- Processing                          *)
(** *===========================================================================*)

let pi_eval (e : exp) : exp list list =
  e |> precomputation |> eval |> postcomputation

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

let pretty_print (e : exp list list) =
  e |> List.iter (fun l -> List.iteri (fun i x ->
                                      if i = List.length l - 1
                                      then Printf.printf "%s\n\t" (to_string x)
                                      else Printf.printf "%s | " (to_string x)) l)

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
        (* Printf.printf "========== Fuel Testing ===========\n";
         * test (fuel 3) test_suite;
         * Printf.printf "========== Top-level Testing ===========\n";
         * test (pi_eval 3) test_suite_2; *)
()
