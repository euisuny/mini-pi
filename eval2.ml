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


(* TODO: Substitute v for x in e, avoiding capture. *)
let rec subst (e : exp) (x : id) (v : id) : exp = e
  match v with
  | Act (chan, act, e1) ->
    let new_chan = if chan = x then nx else chan in
    let new_act =
      begin match act with
        | ASend c -> if c = x then ASend nx else act
        | ARecv c -> if c = x then ARecv nx else act
      end in
    let new_e1 : exp = subst e1 x nx in
    Act (new_chan, new_act, new_e1)
  | Par (e1, e2) ->
    let new_e1 : exp = subst e1 x nx in
    let new_e2 : exp = subst e2 x nx in
    Par (new_e1, new_e2)
  | Bang e1 ->
    let new_e1 : exp = subst e1 x nx in
    Bang (new_e1)
  | Zero -> Zero

(* TODO *)
let rec eval (e : exp) : list exp = e
