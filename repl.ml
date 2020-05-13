(* Top-level read-eval-print loop. *)

exception Quit

(* Current parsed program. *)
let program : Ast.exp option ref = ref None

(* Command handlers. *)

let load (file : string) : unit =
  let ch =
    try open_in file
    with Sys_error s -> failwith ("Cannot open file: " ^ s) in
  let parse : Ast.exp =
      let lexbuf = Lexing.from_channel ch in
      try
        Parser.main Lexer.token lexbuf
      with _ ->
        begin
          close_in ch;
          failwith ("Parsing error at character " ^ string_of_int (Lexing.lexeme_end lexbuf))
        end in
  program := Some parse;
  close_in ch

let list() : unit =
  match !program with
    | Some exp -> print_endline (Ast.to_string exp)
    | None -> failwith "No program loaded"

let run() : unit =
  match !program with
    | Some exp ->
        let value = Eval.pi_eval exp in
        (Ast.pretty_print value)
    | None -> failwith "No program loaded"

let fuel (i : int) : unit =
  match !program with
    | Some exp ->
        let value = Eval.pi_fuel i exp in
        (Ast.pretty_print value)
    | None -> failwith "No program loaded"

let help() : unit =
  print_endline "Available commands are:";
  print_endline "load <file>, list, run, fuel <num>, help, quit"

let quit() : unit =
  print_endline "bye";
  raise Quit

(* main read-eval-print loop *)
let rec repl() : unit =
  print_string ">> ";
  (try
    let input = Str.split (Str.regexp "[ \t]+") (read_line()) in
    match input with
      | [] -> ()
      | cmd :: args ->
        match (cmd, args) with
          | ("load", [filename]) -> load filename
          | ("list", []) -> list()
          | ("run", []) -> run()
          | ("fuel", [x]) -> fuel (int_of_string x)
          | ("quit", []) -> quit()
          | _ -> help()
  with Failure s -> print_endline s);
  repl()

let _ =
  print_endline "PI version 2020.0";
  try repl()
  with Quit -> ()
