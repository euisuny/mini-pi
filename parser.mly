%{
    open Ast
%}

/* Tokens */
%token <string> VAR
%token LPAREN RPAREN
%token BANGR SEND RECV PAR
%token GET
%token NEW ZERO
%token EOF

/* Precedence levels */
%right PAR
%left GET
%right BANGR SEND
%right RECV
%right NEW

%start main
%type <Ast.exp> main

%%

main:
  | exp EOF { $1 }

exp:
  | atomic_exp PAR atomic_exp { Par($1,$3) }
  | atomic_exp { $1 }

atomic_exp :
  | NEW var atomic_exp { New($2,$3) }
  | var BANGR var GET atomic_exp { BangR($1,$3,$5) }
  | var RECV var GET atomic_exp { Recv($1,$3,$5) }
  | var SEND var {Send($1, $3) }
  | ZERO { Zero }
  | LPAREN atomic_exp RPAREN { $2 }

var:
  | VAR { $1 }
%%
