open Ast
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

(* Communications on channels is dealt with by keeping track of a map. *)
module StringMap = Map.Make(String)

open StringMap

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

let list_to_par (e : exp list) : exp =
  List.fold_left (fun x y -> Par (x, y)) Zero e

let rec rebind m (e : exp list) =
  match e with
  | [] -> []
  | h :: t -> let vars = HashSet.values (allv h) in
      if (List.exists (fun s -> String.contains s '~') vars) then
      match find_first_opt (fun x -> String.contains x '~' && List.mem x vars) m with
      | Some (x, n) -> let a = (h :: (take (n - 2) t) |> list_to_par) in
        New (x, a) :: (rebind m (take_last ((List.length t) - n - 1) t))
      | None -> h :: (rebind m t)
      else h :: (rebind m t)

let eval (e : exp) : exp list list =
  let (m, l) = flatten empty e in
  let result = eval_flattened l |> List.map (rebind m) in
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
  let result = fuel_eval l x |> List.map (rebind m) in
  result

(** ============================================================================*)
(** Top-level Evaluator, with Pre-and Post- Processing                          *)
(** *===========================================================================*)

let pi_eval (e : exp) : exp list list =
  e |> precomputation |> eval |> postcomputation

let pi_fuel (i : int) (e : exp) : exp list list =
  e |> precomputation |> fuel i |> postcomputation
