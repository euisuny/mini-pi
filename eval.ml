open Ast
open Hashtbl

type action =
| ASend of var
| ARecv of var

(* Monadic pi expressions *)
type exp =
  | Act of channel * action * exp
  | Par of exp * exp
  | Bang of exp
  | Zero

let rec translate (p : pexp) : exp =
  match p with
  | PSend (c, v, p) -> begin
      match v with
      | [] -> translate p
      | hd :: tl -> Act (c, ASend hd, translate (PSend (c, tl, p)))
      end
  | PRecv (c, v, p) -> begin
      match v with
      | [] -> translate p
      | hd :: tl -> Act (c, ARecv hd, translate (PSend (c, tl, p)))
      end
  | PPar (p, p') -> Par (translate p, translate p')
  | PBang p -> Bang (translate p)
  | PZero -> Zero

(* TODO: Implement capture-avoiding substitution. *)
let subst (v : exp) (x : var) (nx : var) : exp = failwith "unimplemented"

(* Async primitives. *)
type 'a _promise =
    Waiting of ('a, unit) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Await : 'a promise -> 'a
let await p = perform (Await p)

(* Message passing primitives. *)
effect Send : (channel * var) -> unit
let send c v = perform (Send (c,v))

effect Recv : channel -> var
let recv x = perform (Recv x)

let send_table : (channel, var) t = Hashtbl.create 30
let recv_table : (channel, unit) t = Hashtbl.create 30

(* Scheduler: queue of processes. *)
let q = Queue.create ()
let enqueue t = Queue.push t q
let dequeue () =
  if Queue.is_empty q then ()
  else Queue.pop q ()

let rec step (e : exp) : exp =
  match e with
  | Act (c, a, e) -> begin
      match a with
      | ARecv x ->
          let nx = recv c in
          step (subst e x nx)
      | ASend x ->
          let () = send c x in
          step e
    end
  | Par (el, er) ->
      let pl = async (fun () -> el) in
      let pr = async (fun () -> er) in
      let pc = async (fun () -> Par (await pr, await pl)) in
      step (await pc)
  | Bang p ->
      let pr = async (fun () -> p) in
      let pw = async (fun () -> Bang p) in
      Par (await pr, await pw)
  | v -> v

let run main =
  let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
    fun pr main ->
      match main () with
      | v ->
          let l = match !pr with
            | Waiting l -> l
            | _ -> failwith "impossible"
          in
          List.iter (fun k -> enqueue (fun () -> continue k v)) l;
          pr := Done v; dequeue ()
      | effect (Async f) k ->
          let pr = ref (Waiting []) in
          enqueue (fun () -> continue k pr);
          fork pr f
      | effect (Await p) k ->
          begin match !p with
          | Done v -> continue k v
          | Waiting l -> begin
              p := Waiting (k::l);
              dequeue ()
            end
          end
      | effect (Send (c, v)) k ->
          begin match Hashtbl.find_opt recv_table c with
          | None -> Hashtbl.add send_table c v; dequeue ()
          | Some r -> continue k ()
          end
      | effect (Recv c) k ->
          begin match Hashtbl.find_opt send_table c with
          | None -> Hashtbl.add recv_table c (); dequeue ()
          | Some r -> continue k r
          end
  in
  fork (ref (Waiting [])) main
