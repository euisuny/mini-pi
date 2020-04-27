open Ast

(* IY: We probably want either promise or status (need to form a good way to combine
 * the two). *)
(* type 'a _promise =
 *     Waiting of ('a,unit) continuation list
 *   | Done of 'a
 *
 * type 'a promise = 'a _promise ref *)

(* | Paused of int * (int, status) continuation *)

type action =
| ASend of var
| ARecv of var

(* Status of a computation. Send and Recv primitives are "waiting" continuations. *)
type _status =
  Done of var
| Waiting of channel * action * (var, unit) continuation list

type status = _status ref

(* Async primitives *)
effect Async : (unit -> var) -> status
let async f = perform (Async f)

effect Yield : unit
let yield () = perform Yield

(* Used for Send and Recv primitives. *)
effect Await : status -> var
let await p = perform (Await p)

(* Pi-calc primitives *)
effect Par : ((unit -> var) * (unit -> var)) -> unit
let par f = perform (Par f)

effect Bang : (unit -> var) -> unit
let bang f = perform (Bang f)

effect Zero : unit
let zero () = perform Zero

(* TODO: eval should return a normalized pi-calc term. *)
let eval prog =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else Queue.pop run_q () in
  let rec spawn : status -> (unit -> var) -> unit =
    fun pr main ->
      match main () with
      | v -> failwith "unimplemented"
      | effect (Await p) k ->
          begin match !p with
          | Done v -> continue k v
          | Waiting (c, ASend v, f) -> begin
              p := Waiting (c, ASend v, (k::f));
              dequeue ()
            end
          | Waiting (c, ARecv v, f) -> begin
              p := Waiting (c, ASend v, (k::f));
              dequeue ()
            end
          end
      | effect (Par (p, p')) k ->
          async p;
          async p';
          dequeue ()
      | effect (Bang v) k ->
          failwith "Bang case unimplemented"
          (* async p *)
          (* TODO: encode bang and insert into queue. *)
      | effect Zero k ->
          failwith "Zero case unimplemented"
          (* PZero *)
      (* Standard async handling *)
      | effect (Async f) k ->
          let pr = ref (Waiting ("", ASend "", [])) in
          enqueue (fun () -> continue k pr);
          spawn pr f
      | effect Yield k ->
          (* enqueue (continue k); *)
          dequeue ()
  in
  spawn (ref (Waiting ("", ASend "", []))) prog
(* IY: Maybe we want to define mutually recursive handlers? (this one doesn't have
 * to be mutually recursive.) *)
(* and message_passing a b k =
 *   match a (), b () with
 *   | Paused (v1, k1), Paused (v2, k2) ->
 *       message_passing (fun () -> continue k1 v2) (fun () -> continue k2 v1)
 *   | p1, p2 -> par (fun () -> p1) (fun () -> p2) *)
