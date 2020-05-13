open Printf
open Util

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
(** AST Utility Functions                                                       *)
(** *===========================================================================*)
let rec to_string (e : exp) =
  match e with
  | Send (c, v) -> sprintf "%s!%s" c v
  | Recv (c, v, p) -> sprintf "%s?%s = (%s)" c v (to_string p)
  | BangR (c, v, p) -> sprintf "%s?*%s = (%s)" c v (to_string p)
  | Par (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | New (v, p) -> sprintf "new %s (%s)" v (to_string p)
  | Zero -> sprintf "0"

let pretty_print (e : exp list list) =
  e |> List.iter (fun l -> List.iteri (fun i x ->
                                      if i = List.length l - 1
                                      then Printf.printf "%s\n\t" (to_string x)
                                      else Printf.printf "%s | " (to_string x)) l)

(** *===========================================================================*)
(** Variable and Substitution Functions                                         *)
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
