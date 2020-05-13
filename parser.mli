type token =
  | NEW
  | ZERO
  | VAR of (string)
  | LPAREN
  | RPAREN
  | BANGR
  | SEND
  | RECV
  | PAR
  | GET
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
