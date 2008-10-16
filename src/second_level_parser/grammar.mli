open Basic_types;;

type slp_ocamlet =
  | P_FUN of string * (slp_ocamlet list)
  | P_EXP of slp_expression
and slp_expression =
  | P_BASIC of slp_basic_type
  | P_LIST of slp_ocamlet list
  | P_DICT of (string * slp_ocamlet) list
;;

type token =
  | NUMERIC of (Basic_types.slp_numeric)
  | BOOLEAN of (bool)
  | STRING of (string)
  | CHAR of (char)
  | IDENTIFIER of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | NEG
  | AND
  | OR
  | XOR
  | NOT
  | EQL
  | NEQL
  | GT
  | LT
  | GT_EQ
  | LT_EQ
  | CARET
  | OPEN_PAR
  | CLOSE_PAR
  | OPEN_BRK
  | CLOSE_BRK
  | OPEN_SQRBRK
  | CLOSE_SQRBRK
  | COLON
  | SEMICOLON
  | DOT
  | COMMA
  | EOF

val parse_ocamlet :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> slp_ocamlet list
