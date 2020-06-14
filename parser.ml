type token =
  | VAR of (string)
  | LPAREN
  | RPAREN
  | BANGR
  | SEND
  | RECV
  | PAR
  | GET
  | NEW
  | ZERO
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 19 "parser.ml"
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* BANGR *);
  261 (* SEND *);
  262 (* RECV *);
  263 (* PAR *);
  264 (* GET *);
  265 (* NEW *);
  266 (* ZERO *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\005\000\005\000\003\000\001\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\000\000\000\000\008\000\011\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\009\000\004\000\002\000\000\000\007\000\000\000\000\000\
\000\000\005\000\006\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000"

let yysindex = "\005\000\
\014\255\000\000\000\000\014\255\000\255\000\000\000\000\007\000\
\002\255\255\254\008\255\014\255\000\000\014\255\000\255\000\255\
\000\255\000\000\000\000\000\000\009\255\000\000\010\255\014\255\
\014\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\252\255\253\255"

let yytablesize = 24
let yytable = "\011\000\
\003\000\012\000\015\000\016\000\017\000\001\000\013\000\019\000\
\014\000\020\000\018\000\021\000\022\000\023\000\003\000\004\000\
\024\000\025\000\003\000\026\000\027\000\000\000\005\000\006\000"

let yycheck = "\004\000\
\001\001\005\000\004\001\005\001\006\001\001\000\000\000\012\000\
\007\001\014\000\003\001\015\000\016\000\017\000\001\001\002\001\
\008\001\008\001\000\000\024\000\025\000\255\255\009\001\010\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  BANGR\000\
  SEND\000\
  RECV\000\
  PAR\000\
  GET\000\
  NEW\000\
  ZERO\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 26 "parser.mly"
            ( _1 )
# 104 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_exp) in
    Obj.repr(
# 29 "parser.mly"
                              ( Par(_1,_3) )
# 112 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_exp) in
    Obj.repr(
# 30 "parser.mly"
               ( _1 )
# 119 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_exp) in
    Obj.repr(
# 33 "parser.mly"
                       ( New(_2,_3) )
# 127 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_exp) in
    Obj.repr(
# 34 "parser.mly"
                                 ( BangR(_1,_3,_5) )
# 136 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_exp) in
    Obj.repr(
# 35 "parser.mly"
                                ( Recv(_1,_3,_5) )
# 145 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 36 "parser.mly"
                 (Send(_1, _3) )
# 153 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
         ( Zero )
# 159 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_exp) in
    Obj.repr(
# 38 "parser.mly"
                             ( _2 )
# 166 "parser.ml"
               : 'atomic_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
        ( _1 )
# 173 "parser.ml"
               : 'var))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.exp)
;;
