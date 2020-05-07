open Util
open Printf

(* 
   General idea: 
   Use flatten to go through the AST to pull out all top level sends / receives. 
   Iterate this list to build a mapping of receivers and senders (keyed by channel name).
   Find every pairing of senders and receivers communicating on the same channel. For each
   such pairing, apply the reduction rule to that communication to obtain a new AST. Recurse
   on that AST.

   Technicalities:
   To deal with variable bindings, add a preprocessing step where you iterate the AST and find
   all occurences of new. If you find a new operator in the form of (new x).P in the AST, iterate
   over P and replace all occurences of x with a new unique identifier. 

   To deal with bang do the following in flatten. Whenever you encounter a bang, pull out a
   copy of the underlying receive and add it to the list that flatten is building to be used 
   in evaluate. When reducing, keep the bang in the AST.
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
(* IY: TODO See Section 2.7 for behavior of capture avoiding substitution. *)
let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
  (* IY: Invoking `Fresh.next fresh` should give you a fresh variable in this context. *)
  let fresh = Fresh.make (allv e) in
  let fvv = fv e in
  let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
    match e with
    | Send (id1, id2) ->
      let new_id1 = if id1 = replacee then replacer else id1 in
      let new_id2 = if id2 = replacee then replacer else id2 in
      Send(new_id1, new_id2)
    | Recv (id1, id2, e) | BangR (id1, id2, e)->
      (* IY: TODO We need capture-avoiding substitution here. *)
      let new_id1 = if id1 = replacee then replacer else id1 in
      (* Only continue recursing under the Receive if id2 is not the variable being replced *)
      let new_e = if id2 = replacee then e else (subst e replacee replacer) in
      Recv(new_id1, id2, new_e)
    | Par (e1, e2) ->
      let new_e1 : exp = subst e1 replacee replacer in
      let new_e2 : exp = subst e2 replacee replacer in
      Par (new_e1, new_e2)
    (* This case only arises during precomputation, so it's fine to leave the exp as is *)
    | New (id, e) -> New (id, e)
    | Zero -> Zero in
  subst e replacee replacer

let rec eliminate_new (e: exp) : exp =
  let fresh = Fresh.make (allv e) in
  let rec eliminate_new (e : exp) : exp =
    match e with
    | New (id, e) ->
      let new_id = Fresh.next fresh in
      let new_e = subst e id new_id in
      (* Remove the New operator from the tree *)
      eliminate_new new_e
    | Recv (id1, id2, e) -> Recv (id1, id2, eliminate_new e)
    | BangR (id1, id2, e) -> BangR (id1, id2, eliminate_new e)
    | Par (e1, e2) ->
      let new_e1 = eliminate_new e1 in
      let new_e2 = eliminate_new e2 in
      Par(new_e1, new_e2)
    | _ -> e in
  eliminate_new e

(* TODO *)
let rec eval (e : exp) : exp list = [e]
