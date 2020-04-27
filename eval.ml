open Ast

(* IY: We probably want either promise or status (need to form a good way to combine
 * the two). *)
type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

(* status of a computation *)
type status =
  SDone of pexp
| SSend of channel * var * (var, status) continuation
| SRecv of channel * var * (var, status) continuation

(* Pi-calc primitives *)
effect Par : ((unit -> pexp) * (unit -> pexp)) -> unit
let par f = perform (Par f)

effect Bang : (unit -> pexp) -> unit
let bang f = perform (Bang f)

effect Zero : unit
let zero () = perform Zero

(* Async primitives *)
effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Yield : unit
let yield () = perform Yield

effect Await : 'a promise -> 'a
let await p = perform (Await p)

(* Message exchange *)
effect Send : (channel * var * (var, status) continuation) -> unit
let send c v p = perform (Send (c, v, p))

effect Recv : (channel * var * (var, status) continuation) -> pexp
let recv c v p = perform (Recv (c, v, p))

(* TODO: eval should return a normalized pi-calc term. *)
let eval prog =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  let recv_q = Queue.create () in
  let enqueue_recv e = Queue.push e recv_q in
  let find_receiver c = List.find_opt (fun r -> r.channel = c) recv_q in

  let send_q = Queue.create () in
  let enqueue_send e = Queue.push e send_q in
  let find_sender c = List.find_opt (fun s -> s.channel = c) send_q in

  let rec spawn : 'a. 'a promise -> (unit -> 'a) -> unit =
    fun pr main ->
      match main () with
      | () -> failwith "() unimplemented"
      | effect (Send (c, v, f)) k ->
          let pr = ref (SSend (c, v, f)) in
          enqueue (fun () -> continue k pr)
          begin match find_receiver c with
            | Some sender -> failwith "Send receiver case unimplemented"
            | None -> dequeue ()
          end
      | effect (Recv (c, v, f)) k ->
          let pr = ref (Recv (c, v, f)) in
          enqueue (fun () -> continue k pr)
          begin match find_receiver c with
            | Some sender -> failwith "Recv reciever case unimplemented"
            | None -> dequeue  ()
          end
      | effect (Par (p, p')) k ->
          async p
          async p'
          dequeue ()
      | effect (Bang v) k ->
          failwith "Bang case unimplemented"
          (* async p *)
          (* TODO: encode bang and insert into queue. *)
      | effect Zero k -> PZero
      (* Standard async handling *)
      | effect (Async f) k ->
          let pr = ref (Waiting []) in
          enqueue (fun () -> continue k pr);
          spawn pr f
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
  in
  spawn prog
(* IY: Maybe we want to define mutually recursive handlers? (this one doesn't have
 * to be mutually recursive.) *)
and message_passing a b k =
  match a (), b () with
  | Paused (v1, k1), Paused (v2, k2) ->
      message_passing (fun () -> continue k1 v2) (fun () -> continue k2 v1)
  | p1, p2 -> par (fun () -> p1) (fun () -> p2)
