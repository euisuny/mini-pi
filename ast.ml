open Printf

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type var = string
type channel = string

(* The Core Pi-calculus. (Excluding variable hiding, for now) *)
type pexp =
  | Psend of channel * var * pexp
  | Precv of channel * var * pexp
  | Ppar of pexp * pexp
  | Pbang of pexp
  | Pzero

(* Pretty printing expressions. *)
let rec to_string (e : pexp) : string =
  match e with
  | Psend (c, v, p) -> sprintf "%s (%s).%s" c v (to_string p)
  | Precv (c, v, p) -> sprintf "%s <%s>.%s" c v (to_string p)
  | Ppar (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | Pbang p -> sprintf "! %s" (to_string p)
  | Pzero -> sprintf "0"
