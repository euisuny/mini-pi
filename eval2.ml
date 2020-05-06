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

  
(* This function will be used in the precomputation step to eliminate all "new" operators
   as well as during evaluation for the reduction step *)
let rec subst (e : exp) (replacee : id) (replacer : id) : exp =
  match e with
  | Send (id1, id2) ->
    let new_id1 = if id1 = replacee then replacer else id1 in
    let new_id2 = if id2 = replacee then replacer else id2 in
    Send(new_id1, new_id2)
  | Recv (id1, id2, e) ->
    let new_id1 = if id1 = replacee then replacer else id1 in
    (* Only continue recurisng under the Receive if id2 is not the variable being replced *)
    let new_e = if id2 = replacee then e else (subst e replacee replacer) in
    Recv(new_id1, id2, new_e)
  | BangR (id1, id2, e) -> 
    let new_id1 = if id1 = replacee then replacer else id1 in
    let new_e = if id2 = replacee then e else (subst e replacee replacer) in
    BangR(new_id1, id2, new_e)
  | Par (e1, e2) ->
    let new_e1 : exp = subst e1 replacee replacer in
    let new_e2 : exp = subst e2 replacee replacer in
    Par (new_e1, new_e2)
  (* This case only arises during precomputation, so it's fine to leave the exp as is *)
  | New (id, e) -> New (id, e) 
  | Zero -> Zero

(* TODO *)
let rec eval (e : exp) : list exp = e
