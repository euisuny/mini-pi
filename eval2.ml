open Util
open Printf

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

(* Core Pict AST *)
type exp =
  | Send of id * id
  | Recv of id * id * exp
  | BangR of id * id * exp
  | Par of exp * exp
  | New of id * exp
  | Zero

let rec to_string (e : exp) =
  match e with
  | Send (c, v) -> sprintf "%s!%s" c v
  | Recv (c, v, p) -> sprintf "%s?%s = %s" c v (to_string p)
  | BangR (c, v, p) -> sprintf "%s?*%s = %s" c v (to_string p)
  | Par (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | New (v, p) -> sprintf "new %s %s" v (to_string p)
  | Zero -> sprintf "0"

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

(* The substitution function will be used in the precomputation step to eliminate
   all "new" operators as well as during evaluation for the reduction step *)

(* Substitute replacee by replacer in expression e, avoiding capture. *)
let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
  let fresh = Fresh.make (allv e) in
  let fvv = fv e in
  let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
    match e with
    | Send (id1, id2) ->
      let new_id1 = if id1 = replacee then replacer else id1 in
      let new_id2 = if id2 = replacee then replacer else id2 in
      Send(new_id1, new_id2)
    | Recv (id1, id2, e) | BangR (id1, id2, e)->
      let new_id1 = if id1 = replacee then replacer else id1 in
      (* If id2 is the same as the variable being substituted out, generate a
         new name and substitute all occurrences of id2 in the subexpression
         with this new name *)
      let new_id2 = if id2 = replacee then Fresh.next fresh else id2 in
      let new_e = if id2 = replacee then (subst e id2 new_id2) else (subst e replacee replacer) in
      Recv(new_id1, new_id2, new_e)
    | Par (e1, e2) ->
      let new_e1 : exp = subst e1 replacee replacer in
      let new_e2 : exp = subst e2 replacee replacer in
      Par (new_e1, new_e2)
    (* This case only arises during precomputation, so it's fine to leave the
    exp as is *)
    (* TODO: capture-avoiding substitution needs to be done here.. *)
    | New (id, e) -> New (id, e)
    | Zero -> Zero in
  subst e replacee replacer

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

(** *2. Reducing message passing ======================== *)

(* To deal with bang do the following in flatten. Whenever you encounter a bang,
  * pull out a copy of the underlying receive and add it to the list that flatten
  * is building to be used in evaluate. When reducing, keep the bang in the AST. *)
let rec flatten (e : exp) : exp list =
  match e with
  | Par (e1, e2) -> flatten e1 @ flatten e2
  | BangR (id1, id2, e') -> Recv (id1, id2, e') :: [e]
  | e -> [e]

(* ===== Utility functions ======= *)
(* Communications on channels is dealt with by keeping track of a map. *)
module StringMap = Map.Make(String)

open StringMap

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

(* ===== Keeping track of channels ======= *)
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

(* ===== Evaluation of processed expression ====== *)
(* This function takes care of a single "evaluation" of a pi-calc expression,
 * assuming that it has already been processed (i.e. no "New" operators) *)
let rec eval (e : exp) : exp list list =
  let flattened_list = flatten e in
  let flattened_list_indexed = List.mapi (fun i x -> (i, x)) flattened_list in

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
  List.map (fun (i1, i2) -> swap_message_pass flattened_list i1 i2) pairs_communications


(** *===========================================================================*)
(** Testing                                                                     *)
(** *===========================================================================*)

(** *Testing message passing evaluation ======================== *)

let pretty_print (e : exp list list) =
  e |> List.iter (fun l -> List.iteri (fun i x -> if i = List.length l - 1
                                      then Printf.printf "%s\n\t" (to_string x)
                                      else Printf.printf "%s | " (to_string x)) l)

let e1 = (Par (Send ("x", "hi" ), Recv ("x", "y", Zero)))
let e2 = (Par (Send ("x", "hi" ), Par (Send ("x", "bye"), Recv ("x", "y", Zero))))
let e3 = (Par (Send ("x", "hi" ), BangR ("x", "y", Zero)))

let test_suite = [e1; e2; e3]

let test (e : exp list) =
  let test_eval (e : exp) =
    Printf.printf "Original expression: \n \t %s \n" (to_string e);
    Printf.printf "Reduced expressions: \n \t";
    eval e |> pretty_print;
    Printf.printf "\n" in
  List.iter test_eval e

let _ = test test_suite

(** *Testing pre and post processing functions ======================== *)

let _ =
  let example1 =
    Par(
      Par(Send("x", "y"),
          Recv("y", "z",
               New("z",
                   New("z",
                       Par(
                         Send("x", "y"),
                         Recv("y", "z", Zero)
                       )
                      )
                  )
              )
         ),
      New("y",
          Par(Send("x", "y"), Recv("y", "z", Zero))
         )
    ) in
  let example1_precomputed = precomputation example1 in
  let example1_postcomputed = List.hd (List.hd (postcomputation [[example1]])) in

  print_endline ("orig: " ^ to_string example1) ; 
  print_endline ("pre: " ^ to_string example1_precomputed) ; 
  print_endline ("post: " ^ to_string example1_postcomputed)
