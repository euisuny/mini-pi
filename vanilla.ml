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

(* Get the free variables of an expression. *)
let fv (e : exp) (x : id) : id HashSet.t =
  let h = HashSet.make() in
    let rec fv (bv : id list) (e : exp) : unit =
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

(* TODO: Substitute v for x in e, avoiding capture. *)
let rec subst (e : exp) (x : id) (v : id) : exp = e

module type Monad =
  sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  end

let rec join : 'a list list -> 'a list =
  function
  | nil -> []
  | xs :: xss -> xs @ (join xss)

module Nondet : Monad =
  sig
    type 'a t
    let return x
    val (>>=) m f = join (List.map f m)
    val (>>) (m1 : 'a t) (m2 : 'a t) : 'a t = m1 @ m2
  end

module Nondet : Monad = struct
  type 'a t = 'a list
  let return x = [x]
  let (>>=) m f = join (List.map f m)
  let (>>) (m1 : 'a t) (m2 : 'a t) : 'a t = m1 @ m2
end

open Nondet

(* TODO Use "fuel" to define how many evaluation steps we would like to take. *)
let rec cong (e : exp) : exp t =
  match e with
  | Par (Par (l, m), r) -> Par (l, Par (m, r)) |> return
  (* TODO Check if x is a free variable in e. *)
  | Par (New (x, e), r) -> New (x, (Par (e, r))) |> return
  | Par (l, r) -> Par (r, l) |> return
  | _ -> e |> return
and eval (e : exp) : exp t =
  match e with
  | Par (l, r) ->
    (eval l >>= (fun x -> Par (x , r) |> return)) >>
    (eval r >>= (fun x -> Par (l, r) |> return))
  | Par (Send (c, v), Recv (c', x, e')) ->
      (if (c = c') then subst e' x v else e) |> return
  | Par (Send (c, v), BangR(c', x, e')) ->
      (if (c = c') then Par (subst e' x v, BangR (c', x, e')) else e) |> return
  | e -> cong e

let prog = Par (Send ("x", "v"), Par (Recv ("x", "k", Zero), Zero))
let main = Printf.printf "%s" (prog |> eval |> eval |> eval |> to_string)
