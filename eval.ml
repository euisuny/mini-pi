open Ast

type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

(* status of a computation *)
type status =
  Done of pexp
| Send of channel * var * (var, status) continuation
| Recv of channel * var * (var, status) continuation

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
effect Send : (channel * var * (var, status) continuation) -> unit
let send c v p = perform (Send (c, v, p))

effect Recv : (channel * var * (var, status) continuation) -> pexp
let recv c v p = perform (Recv (c, v, p))

effect Xchg : int -> int
let xchg n = perform (Xchg n)

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
        let pr = ref (Paused (c, v, f))
        enqueue (fun () -> continue k pr)
        begin match find_receiver channel with
          | Some sender -> failwith "Send receiver case unimplemented"
          | None -> dequeue ()
        end
      | effect (Recv (channel, v, process)) k ->
        let pr = ref (Paused (c, v, f))
        enqueue (fun () -> continue k pr)
        begin match find_receiver channel with
          | Some sender -> failwith "Recv reciever case unimplemented"
          | None -> dequeue ()
        end
      | effect (Par (p, p')) k -> async p; async p'; failwith "Par cont unimplemented"
      | effect (Bang v) k -> failwith "Bang unimplemented"
      | effect Zero k -> PZero
  in
  spawn prog
