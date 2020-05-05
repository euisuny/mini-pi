open Util
open Printf

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

(* TODO *)
let rec eval (e : exp) : list exp = e
