open Printf
open Util

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type var = string
type channel = string

(* The Core Polyadic Pi-calculus. (Excluding variable hiding, for now) *)
type pexp =
  | PSend of channel * var list * pexp
  | PRecv of channel * var list * pexp
  | PPar of pexp * pexp
  | PBang of pexp
  | PZero

(* Pretty printing expressions. *)
let rec to_string (e : pexp) : string =
  match e with
  | PSend (c, v, p) -> sprintf "%s (%s).%s" c (String.concat " " v) (to_string p)
  | PRecv (c, v, p) -> sprintf "%s <%s>.%s" c (String.concat " " v) (to_string p)
  | PPar (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | PBang p -> sprintf "! %s" (to_string p)
  | PZero -> sprintf "0"

(* TODO: Not terribly relevant before we have the "new" operator. *)
(* Get free variables of an expression. *)
(* IY : How should we treat channel names? *)
let fv (e : pexp) : var HashSet.t =
  let h = HashSet.make() in
  let rec fv (bv : var list) (e : pexp) : unit =
    match e with
    | PSend (c, v, p) -> fv bv e
    | PRecv (c, v, p) -> fv (v @ bv) e
    | PPar (p, p') -> fv bv p; fv bv p'
    | PBang p -> fv bv p
    | PZero -> () in
  fv [] e; h

(* Get all variables of an expression. *)
let allv (e : pexp) : channel HashSet.t = failwith "unimplemented"

(* Substitute v for x in e, avoiding capture. *)
let subst (v : pexp) (x : var) (nx : var) : pexp = failwith "unimplemented"
