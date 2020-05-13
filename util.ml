(* A few utilities. *)


(*****************************************************
 * HashSet, like in Java!
 *****************************************************)
module type HashSet = sig
  type 'a t
  val make : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val mem : 'a t -> 'a -> bool
  val size : 'a t -> int
  val values : 'a t -> 'a list
end

module HashSet : HashSet = struct
  type 'a t = ('a, 'a) Hashtbl.t
  let make() : 'a t = Hashtbl.create 16
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let add (h : 'a t) (x : 'a) =
    if mem h x then () else Hashtbl.add h x x
  let remove (h : 'a t) (x : 'a) =
    while Hashtbl.mem h x do
      Hashtbl.remove h x
    done
  let size (h : 'a t) : int = Hashtbl.length h
  let values (h : 'a t) : 'a list =
    Hashtbl.fold (fun x y v -> y :: v) h []
end

(*****************************************************
 * Stream of strings in length-lexicographic order,
 * used to create new variable names.
 *****************************************************)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref

  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t

  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end

(*****************************************************
 * A source of fresh variable names, avoiding a given
 * set of strings.
 *****************************************************)
module type Fresh = sig
  type t
  val make : string HashSet.t -> t
  val avoid : t -> string HashSet.t
  val next : t -> string
end

module Fresh : Fresh = struct
  type t = (string HashSet.t * LexStream.t) ref

  let make (avoid : string HashSet.t) : t = ref (avoid, LexStream.make())

  let avoid (s : t) : string HashSet.t = fst (!s)

  let next (s : t) : string =
    let (avoid, stream) = !s in
    let rec check n = if HashSet.mem avoid n then check (LexStream.next stream) else n in
    check (LexStream.next stream)
end


(*****************************************************
 * List-related Utility Functions.
 *****************************************************)
(* Remove duplicates in a list using HashSet. *)
let remove_duplicates (l : 'a list) : 'a list =
  let h = HashSet.make () in
  List.iter (fun x -> HashSet.add h x) l;
  h |> HashSet.values

(* Permutations from https://gist.github.com/Bamco/6151962 *)
(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst =
  match lst with
  | [] -> [[x]]
  | hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl))

let rec permutations lst =
  match lst with
  | hd::tl -> List.concat (List.map (interleave hd) (permutations tl))
  | _ -> [lst]


let rec take n list =
  if n > 0 then
    match list with
    | [] -> failwith "Not enough elements in list"
    | x :: xs -> x :: (take (n - 1) xs)
  else []

let take_last n list =
  let rec take_last n list =
    if n = 0 then list
    else
      match list with
      | [] -> []
      | x :: xs -> (take_last (n - 1) xs) in
  take_last (List.length list - n) list
