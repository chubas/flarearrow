type slp_numeric =
  | Int of int
  | Float of float
;;

type slp_basic_type =
  | Numeric of slp_numeric
  | Boolean of bool
  | String of string
  | Char of char
;;

type slp_ocamlet =
  | P_FUN of string * (slp_ocamlet list)
  | P_EXP of slp_expression
and slp_expression =
	| P_BASIC of slp_basic_type
	| P_LIST of slp_ocamlet list
  | P_DICT of (string * slp_ocamlet) list
;;