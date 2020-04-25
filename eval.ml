open Ast

effect Send : (channel * var * pexp) -> unit
let send c v p = perform (Send (c, v, p))

effect Recv : (channel * var * pexp) -> pexp
let recv f = perform (Recv f)

effect Par : (pexp -> pexp -> unit) -> unit
let par f = perform (Par f)

effect Bang : var -> unit
let bang v = perform (Bang v)

effect Zero : unit
let zero () = perform Zero


let eval prog =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  
  (* A queue for blocked procceses waiting to receive and similarly for processes waiting to send *)
  (* Good to separate from the run_q so that we don't have to search through the run_q, ignoring
     blocked processes *)
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
      (* Find some receiver that is waiting to receive on the channel you are trying to send on *)
      begin match find_receiver channel with
        | Some receiver ->
          (* want to unblock the receiver and send it the process *)
          (* add both the receiver and sender back to the run_q *)
          (* should the interpreter handle the actual substituting? I think so. in which case
             just call continue to return the process to the receiver *)
          failwith "Send receiver case unimplemented"
        | None -> (* block this process since there is no receiver yet *)
          (* enqueue_send {k, channel} *) (* what to keep track of in the send_q? *)
          failwith "Send no receiver case unimplemented"
      end ;
      dequeue()
      
    | effect (Recv f) k -> failwith "Recv unimplemented"
    | effect (Par f) k -> failwith "Par unimplemented"
    | effect (Bang v) k -> failwith "Bang unimplemented"
    | effect Zero k -> failwith "Zero unimplemneted"
  in
  spawn prog
