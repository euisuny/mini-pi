open Ast

type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

(* Pi-calc primitives *)
effect Par : ((unit -> pexp) * (unit -> pexp)) -> unit
let par f = perform (Par f)

(* Async primitives *)
effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Yield : unit
let yield () = perform Yield

effect Await : 'a promise -> 'a
let await p = perform (Await p)

effect Bang : var -> unit
let bang v = perform (Bang v)

effect Zero : unit
let zero () = perform Zero

(* Message exchange *)
effect Send : (channel * var * pexp) -> unit
let send c v p = perform (Send (c, v, p))

effect Recv : (channel * var * pexp) -> pexp
let recv c v p = perform (Recv (c, v, p))

(* TODO: Use Xchag to implement Send, Recv *)
effect Xchg : int -> int

(* status of a computation *)
(* We might want to do something similar to here, or use promises at the least to
   implement a concurrent handling. *)
type status =
  Done
| Paused of int * (int, status) continuation

(* step through [f v] until either termination or pauses on Xchg *)
let step f v () =
  match f v with
  | _ -> Done
  | effect (Xchg m) k -> Paused (m, k)

let rec message_passing a b =
match a (), b () with
| Done, Done -> ()
| Paused (v1, k1), Paused (v2, k2) ->
    run_both (fun () -> continue k1 v2) (fun () -> continue k2 v1)
| _ -> failwith "improper synchronization"

let eval prog =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  (* A queue for blocked procceses waiting to receive and similarly for processes
   * waiting to send.
   * Good to separate from the run_q so that we don't have to search through the
   * run_q, ignoring blocked processes *)
  let recv_q = Queue.create () in
  let enqueue_recv e = Queue.push e recv_q in
  let find_receiver c = List.find_opt (fun r -> r.channel = c) recv_q in

  let send_q = Queue.create () in
  let enqueue_send e = Queue.push e send_q in
  let find_sender c = List.find_opt (fun s -> s.channel = c) send_q in

  let rec spawn f =
    match f () with
    | () -> failwith "() unimplemented"
    | effect (Send (channel, v, process)) k ->
      (* Find some receiver that is waiting to receive on the channel you are trying
       * to send on *)
      begin match find_receiver channel with
        | Some receiver ->
          (* want to unblock the receiver and send it the process *)
          (* add both the receiver and sender back to the run_q *)
          (* should the interpreter handle the actual substituting? I think so. in
           * which case just call continue to return the process to the receiver *)
          failwith "Send receiver case unimplemented"
        | None -> (* block this process since there is no receiver yet *)
          (* enqueue_send {k, channel} *) (* what to keep track of in the send_q? *)
          failwith "Send no receiver case unimplemented"
      end ;
      dequeue ()
    | effect (Recv (channel, v, process)) k ->
      begin match find_sender channel with
        | Some sender -> failwith "Send receiver case unimplemented"
        | None -> failwith "Send no receiver case unimplemented"
      end
    | effect (Par (p, p')) k -> async p; async p'; failwith "Par cont unimplemented"
    | effect (Bang v) k -> failwith "Bang unimplemented"
    | effect Zero k -> PZero
  in
  spawn prog
