%{
    open Ast
%}

%token NEW ZERO
%token <string> VAR
%token LPAREN RPAREN
%token BANGR SEND RECV PAR
%token GET
%token EOF

%start main
%type <Ast.exp> main

%%

main:
  | exp EOF { $1 }

exp:
  | NEW var exp { New($2,$3) }
  | atomic_exp PAR atomic_exp { Par($1,$3) }
  | var BANGR var GET exp { BangR($1,$3,$5) }
  | var RECV var GET exp { Recv($1,$3,$5) }
  | atomic_exp { $1 }

atomic_exp :
  | var SEND var {Send($1, $3) }
  | ZERO { Zero }
  | LPAREN exp RPAREN { $2 }

var:
  | VAR { $1 }
%%
