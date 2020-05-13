{
  open Parser
}

let alph = ['a'-'z' 'A'-'Z']
let rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'']*

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf } (* skip whitespace *)
  | "new" { NEW }
  | "zero" { ZERO }
  | alph rest as id { VAR id }
  | "?*"   { BANGR }
  | '!'    { SEND }
  | '?'    { RECV }
  | '|'    { PAR }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '='    { GET }
  | eof    { EOF }
