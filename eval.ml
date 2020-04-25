open Ast

effect Send : (channel -> var -> pexp -> unit) -> unit
let send f = perform (Send f)

effect Recv : (channel -> var -> pexp -> unit) -> unit
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
  let rec spawn f =
    match f () with
    | () -> failwith "() unimplemented"
    | effect (Send f) k -> failwith "Send unimplemented"
    | effect (Recv f) k -> failwith "Recv unimplemented"
    | effect (Par f) k -> failwith "Par unimplemented"
    | effect (Bang v) k -> failwith "Bang unimplemented"
    | effect Zero k -> failwith "Zero unimplemneted"
  in
  spawn prog
