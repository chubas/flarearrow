%{

type flp_token =
  | COMMENT of string
  | EXPRESSION of string
  | RAWTEXT of string
  | CONDITIONAL of (string * (flp_token list) * (flp_token list))
  | LOOP of (string * (flp_token list))
;;
%}

%token <string> Comment
%token <string> Expression
%token <string> RawText
%token <string> ControlIf
%token <string> ControlFor
%token ControlElse
%token EndFor
%token EndIf
%token Eof

  /* Entry point */
%start parse_template
%type <flp_token list> parse_template

%%

parse_template: /* list of flp_token */
  | Eof                           { [] }
  | items Eof                     { $1 }
;

items:
  | item items      { $1::$2 }
  | item            { [$1] }

item:  /* flp_token */
  | Comment         { COMMENT $1 }
  | Expression      { EXPRESSION $1 }
  | RawText         { RAWTEXT $1 }
  | if_expression   { CONDITIONAL $1 }
  | for_expression  { LOOP $1 }
;

if_expression: /* string * flp_token list * flp_token_list */
  | ControlIf items ControlElse items EndIf  { $1, $2, $4 }
  | ControlIf items EndIf                    { $1, $2, [] }
;

for_expression: /* string * flp_token_list */
  | ControlFor items EndFor                  { $1, $2 }
;
