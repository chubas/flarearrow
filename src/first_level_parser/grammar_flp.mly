%{

open Position;;

type flp_token =
  | COMMENT of (string * Position.flp_position)
  | EXPRESSION of (string * Position.flp_position)
  | RAWTEXT of (string * Position.flp_position)
  | CONDITIONAL of ((string * Position.flp_position) * (flp_token list) * (flp_token list))
  | LOOP of ((string * (string list) * Position.flp_position) * (flp_token list))
;;

(*
type _fpl_token = 
	| P_COMMENT of string * Position.flp_position
	| P_EXPRESSION of string * Position.flp_position
	| P_RAWTEXT of string * Position.flp_position
	| P_CONTROLIF of string * Position.flp_position
	| P_CONTROLFOR of string * (string list) * Position.flp_position
	| P_CONTROLELSE of Position.flp_position
	| P_CONTROLENDIF of Position.flp_position
	| P_CONTROLENDFOR of Position.flp_position
  | EOF
;;
*)
%}

%token <string * Position.flp_position> P_COMMENT
%token <string * Position.flp_position> P_EXPRESSION
%token <string * Position.flp_position> P_RAWTEXT
%token <string * Position.flp_position> P_CONTROLIF
%token <string * (string list) * Position.flp_position> P_CONTROLFOR
%token <Position.flp_position> P_CONTROLELSE
%token <Position.flp_position> P_CONTROLENDFOR
%token <Position.flp_position> P_CONTROLENDIF
%token EOF

  /* Entry point */
%start parse_template
%type <flp_token list> parse_template

%%

parse_template: /* list of flp_token */
  | EOF                           { [] }
  | items EOF                     { $1 }
;

items:
  | item items      { $1::$2 }
  | item            { [$1] }

item:  /* flp_token */
  | P_COMMENT         { COMMENT $1 }
  | P_EXPRESSION      { EXPRESSION $1 }
  | P_RAWTEXT         { RAWTEXT $1 }
  | if_expression     { CONDITIONAL $1 }
  | for_expression    { LOOP $1 }
;

if_expression: /* string * flp_token list * flp_token_list */
  | P_CONTROLIF items P_CONTROLELSE items P_CONTROLENDIF  { $1, $2, $4 }
  | P_CONTROLIF items P_CONTROLENDIF                      { $1, $2, [] }
;

for_expression: /* string * flp_token_list */
  | P_CONTROLFOR items P_CONTROLENDFOR                  { $1, $2 }
;

