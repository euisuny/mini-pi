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
let rec subst (v : exp) (x : var) (nx : var) : exp =
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

(* Async primitives. *)
type 'a _promise =
    Waiting of ('a, exp) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Yield : unit
let yield () = perform Yield

effect Await : 'a promise -> 'a
let await p = perform (Await p)

(* Message passing primitives. *)
effect Send : (channel * 'a) -> unit
let send c v = perform (Send (c,v))

effect Recv : channel -> 'a
let recv x = perform (Recv x)

(* Scheduler: queue of processes. *)
let q = Queue.create ()
let enqueue (t : unit -> exp) = Queue.push t q
let dequeue () : exp =
  if Queue.is_empty q then failwith "Queue is empty"
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
      enqueue (fun () -> Bang p);
      Par (await pr, Bang p)
  | v -> v

let rec eval prog =
  let rec fork : 'a. 'a promise -> (unit -> 'a) -> exp =
    fun pr main ->
      match main () with
      | v ->
          let l = match !pr with
            | Waiting l -> l
            | _ -> failwith "impossible"
          in
          List.iter (fun k -> enqueue (fun () -> continue k v)) l;
          pr := Done v;
          dequeue ()
      | effect (Async f) k ->
          let pr = ref (Waiting []) in
          enqueue (fun () -> continue k pr);
          fork pr f
      | effect Yield k ->
          enqueue (continue k);
          dequeue ()
      | effect (Await p) k ->
          begin match !p with
          | Done v -> continue k v
          | Waiting l -> begin
              p := Waiting (k::l);
              dequeue ()
            end
          end
      | effect (Send _) k -> failwith "Unimplemented send" (* TODO *)
      | effect (Recv _) k -> failwith "Unimplemented recv" (* TODO *)
  in
  fork (ref (Waiting [])) eval

(* let tb = Hashtbl.create 20
 * let enqueue (c : channel) (v : action option * 'a) = Hashtbl.add tb c v
 * let match_action (c : channel) (v : action option * 'a) = begin
 *   match (Hashtbl.find_opt tb c, v) with
 *   | Some (Some (ASend n), pl), (Some (ARecv n'), pr) -> begin
 *     match n = n' with
 *     | true -> Hashtbl.remove tb c; Some (pl, pr)
 *     | false -> None end
 *   | _, _ -> None end *)

(* TODO: eval should return a normalized pi-calc term. *)
(* let eval prog =
 *   let rec spawn : status -> (unit -> (action option * (var, unit) continuation)) -> unit =
 *     fun pr main ->
 *       match main () with
 *       | v -> failwith "unimplemented"
 *       | effect (Await p) k ->
 *           begin match !p with
 *           | Done v -> continue k v
 *           | Waiting (c, Some (ASend v), Some f) -> begin
 *               match match_action c (Some (ASend v), f) with
 *               | Some (kl, kr) -> (continue kl v); (continue kr v)
 *               | _ -> p := Waiting (c, Some (ASend v), Some f)
 *             end
 *           | Waiting (c, Some (ARecv v), Some f) -> begin
 *               match match_action c (Some (ARecv v), f) with
 *               | Some (kl, kr) -> (continue k v); (continue k v)
 *               | _ -> p := Waiting (c, Some (ARecv v), Some f)
 *             end
 *           end
 *       | effect (Bang v) k ->
 *           failwith "Bang case unimplemented"
 *           (\* async p *\)
 *           (\* TODO: encode bang and insert into queue. *\)
 *       | effect Zero k ->
 *           failwith "Zero case unimplemented"
 *           (\* PZero *\)
 *       (\* Standard async handling *\)
 *       | effect (Async f) k ->
 *           let pr = ref (Waiting ("", None, None)) in
 *           (\* enqueue "" (continue k) *\)
 *           spawn pr f
 *       (\* | effect Yield k ->
 *        *     enqueue (continue k);
 *        *    dequeue () *\)
 *   in
 *   spawn (ref (Waiting ("", None, None))) prog
 * (\* IY: Maybe we want to define mutually recursive handlers? (this one doesn't have
 *  * to be mutually recursive.) *\) *)
