open Printf

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type var = string
type channel = string

(* The Core Pi-calculus. (Excluding variable hiding, for now) *)
type exp =
  | Send of channel * var * exp
  | Recv of channel * var * exp
  | Par of exp * exp
  | Bang of var
  | Zero

(* Trace of program. *)
type send = Send of trace
and receive = Recv of (trace -> trace)

and trace = ((send list) * (receive list)) SMap.t * (var list)

(* Pretty printing expressions. *)
let rec to_string (e : exp) : string =
  match e with
  | Send (c, v, p) -> sprintf "%s (%s).%s" c v (to_string p)
  | Recv (c, v, p) -> sprintf "%s <%s>.%s" c v (to_string p)
  | Par (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | Bang v -> sprintf "! %s" v
  | Zero -> sprintf "0"
