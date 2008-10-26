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
