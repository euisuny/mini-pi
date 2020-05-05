open Util

type var = string

type exp =
  | Send of var * var
  | Recv of var * var * exp
  | BangR of var * var * exp
  | Par of exp * exp
  | New of var * exp
  | Zero

let fv (e : exp) (x : var) : var HashSet.t =
  let h = HashSet.make() in
    let rec fv (bv : var list) (e : exp) : unit =
      match e with
      | Send (c, x) ->
          begin if List.mem x bv then () else HashSet.add h x end;
          begin if List.mem c bv then () else HashSet.add h c end
      | (Recv (c, x, e) | BangR (c, x, e)) ->
          begin if List.mem x bv then () else HashSet.add h x end;
          begin if List.mem c bv then () else HashSet.add h c end;
          fv bv e
      | New (x, e) -> fv (x :: bv) e
      | Par (e1, e2) -> fv bv e1; fv bv e2
      | Zero -> () in
    fv [] e; h


let rec subst (e : exp) (x : var) (v : var) : exp = failwith "Unimplemented."

let rec cong (e : exp) : exp =
  match e with
  | Par (l, r) -> Par (r, l)
  | Par (Par (l, m), r) -> Par (l, Par (m, r))
  | Par (New (x, e), r) -> New (x, (Par (e, r)))
and eval (e : exp) : exp =
  match e with
  | Par (Send (c, v), Recv (c', x, e')) -> if (c = c') then subst e' x v else e
  | Par (Send (c, v), BangR(c', x, e')) -> if (c = c') then Par (subst e' x v, BangR (c', x, e')) else e
  | Par (l, r) -> Par (eval l, r)
  | Par (l, r) -> Par (r, eval l)
  | e -> cong e
