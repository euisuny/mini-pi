open Printf
open Util

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type var = string
type channel = string

(* The Core Polyadic Pi-calculus. (Excluding variable hiding, for now) *)
type pexp =
  | Psend of channel * var list * pexp
  | Precv of channel * var list * pexp
  | Ppar of pexp * pexp
  | Pbang of pexp
  | Pzero

(* Pretty printing expressions. *)
let rec to_string (e : pexp) : string =
  match e with
  | Psend (c, v, p) -> sprintf "%s (%s).%s" c (String.concat " " v) (to_string p)
  | Precv (c, v, p) -> sprintf "%s <%s>.%s" c (String.concat " " v) (to_string p)
  | Ppar (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | Pbang p -> sprintf "! %s" (to_string p)
  | Pzero -> sprintf "0"

(* Get free variables of an expression. *)
let fv (e : pexp) : var HashSet.t =
  let h = HashSet.make() in
  let rec fv (bv : var list) (e : pexp) : unit =
    match e with
    | Psend (c, v, p) -> fv (v @ bv) e
    | Precv (c, v, p) -> fv bv e
    | Ppar (p, p') -> fv bv p; fv bv p'
    | Pbang p -> fv bv p
    | Pzero -> () in
  fv [] e; h

(* Get all variables of an expression. *)
let allv (e : pexp) : channel HashSet.t = failwith "unimplemented"

(* Substitute v for x in e, avoiding capture. *)
let subst (v : pexp) (x : channel) (e : pexp) : pexp = failwith "unimplemented"
