open Ast
open Util
open Printf
open Hashtbl

let debug = Printf.printf

type action =
| ASend of var
| ARecv of var

(* Monadic pi expressions *)
type exp =
  | Act of channel * action * exp
  | Par of exp * exp
  | Bang of exp
  | Zero

(* Pretty printing expressions. *)
let rec to_string (e : exp) : string =
  match e with
  | Act (c, ASend v, p) -> sprintf "%s <%s>.%s" c v (to_string p)
  | Act (c, ARecv v, p) -> sprintf "%s (%s).%s" c v (to_string p)
  | Par (p, p') -> sprintf "%s | %s" (to_string p) (to_string p')
  | Bang p -> sprintf "! %s" (to_string p)
  | Zero -> sprintf "0"

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
    Waiting of ('a, unit) continuation list
  | Done of 'a

type 'a promise = 'a _promise ref

effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Await : 'a promise -> 'a
let await p = perform (Await p)

effect Yield : unit
let yield () = perform Yield(* Message passing primitives. *)

effect Send : (channel * var) -> unit
let send c v = perform (Send (c,v))

effect Recv : channel -> var
let recv x = perform (Recv x)


let rec step (e : exp) : exp =
  debug "Stepping: %s \n" (to_string e);
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
      let pl = async (fun () -> step el) in
      let pr = async (fun () -> step er) in
      Par (await pr, await pl)
  | Bang p ->
      let pr = async (fun () -> step p) in
      let pw = async (fun () -> Bang p) in
      Par (await pr, await pw)
  | v -> v

let run main =
  let send_table : (channel, var) t = Hashtbl.create 30 in
  let recv_table : (channel, unit) t = Hashtbl.create 30 in

  (* Scheduler: queue of processes. *)
  let q = Queue.create () in
  let enqueue t = Queue.push t q in
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q () in

  let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
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
      | effect (Await p) k ->
          begin match !p with
          | Done v -> continue k v
          | Waiting l -> begin
              p := Waiting (k::l);
              dequeue ()
            end
          end
      | effect Yield k ->
          enqueue (continue k);
          dequeue ()
      | effect (Send (c, v)) k ->
          begin match Hashtbl.find_opt recv_table c with
          | None -> Hashtbl.add send_table c v; enqueue (continue k); dequeue ()
          | Some r -> continue k ()
          end
      | effect (Recv c) k ->
          begin match Hashtbl.find_opt send_table c with
          | None -> Hashtbl.add recv_table c (); dequeue ()
          | Some r -> continue k r
          end
  in
  fork (ref (Waiting [])) main;
  Hashtbl.iter (fun k v -> debug "send_table [Key: %s, Value: %s]\n" k v) send_table;
  Hashtbl.iter (fun k () -> debug "recv_table [Key: %s, Value: ()]\n" k) recv_table

let prog = Par (Act ("hi", ASend "msg", Zero), Act ("hi", ARecv "msg", Zero))
let foo () = let p = step prog in p; debug "P-result: %s\n" (to_string p); p

let f = run foo
