open Basic_types;;
open Grammar;;
open Second_level_parser;;

(* Reduces type that eliminates functions to basic types *)
type expression =
  | EXP  of slp_basic_type
  | LIST of expression list
  | DICT of (string * expression) list
  | NULL
;;

exception BoundFunctionError;;
exception NotDeclaredFunctionError;;
exception InvalidNumberOfParameters of (int * int);;
exception InvalidParameterType of (int * string);;
exception UnknownException;;

(* Hashtable that contains custom function definitions *)
let _P_FUNS_HASHTABLE = Hashtbl.create 50;;

(*
 * Defines a custom function, by associating its name with a function in 
 * the hash table
 * The function should receive as parameter a list of slp_expressions and
 * return an slp_expression
 *)
let _DEFINE_P_FUN name funct =
  if Hashtbl.mem _P_FUNS_HASHTABLE name then
    raise BoundFunctionError
  else
    Hashtbl.add _P_FUNS_HASHTABLE name funct;;

(*
 * Evals a function by applying the definition stored in the hash table
 * or throwing an exception if the function is not defined
 * The parameters are each evaluated in case any of them is a function itself.
 *)
let rec _EVAL_P_FUN identifier params = (try
    (Hashtbl.find _P_FUNS_HASHTABLE identifier) (reduce_eval_list params)
  with 
  | Not_found -> raise NotDeclaredFunctionError
)
(* 
 * Transform from slp_ocamlets to expressions, with all
 * FUNCTION evaluated
 *)
and reduce_eval_list lst =
  List.map (function
    | P_EXP (P_LIST  l)   -> LIST (reduce_eval_list l)
    | P_EXP (P_DICT  d)   -> DICT (reduce_eval_dict d)
    | P_EXP (P_BASIC b)   -> EXP b
    | P_FUN (str, par)    -> _EVAL_P_FUN str par
  ) lst

and reduce_eval_dict dict =
  List.map (function
    | id, P_EXP (P_LIST  l) -> id, LIST (reduce_eval_list l)
    | id, P_EXP (P_DICT  d) -> id, DICT (reduce_eval_dict d)
    | id, P_EXP (P_BASIC b) -> id, EXP b
    | id, P_FUN (str,  par) -> id, _EVAL_P_FUN str par
  ) dict
;;

(* Alias for reduce_eval_list *)
let rl = reduce_eval_list;;

(* Alias for reduce_eval_dict *)
let rd = reduce_eval_dict;;

(* ********************** *)
(* Add standard functions *)
(* ********************** *)

_DEFINE_P_FUN "strlen" 
  (function
    | [EXP (String s)]   -> EXP (Numeric (Int (String.length s)))
    | [EXP (Char   s)]   -> EXP (Numeric (Int 1))
    | [_]       -> raise (InvalidParameterType (1, "string or character"))
    | other     -> raise (InvalidNumberOfParameters (1, List.length other)) 
  )
;;

(* List or string access, i.e.: "foobar".[1] or [1;2;3].[1] *)
_DEFINE_P_FUN "_list_at"
  (function
    | [LIST l; EXP (Numeric (Int i))] ->
      List.nth l i
    | [EXP (String s); EXP (Numeric (Int i))] ->
      EXP (Char s.[i])
    | [_; EXP (Numeric (Int _))] ->
      raise (InvalidParameterType (1, "list or string"))
    | [_; _] ->
      raise (InvalidParameterType (2, "integer"))
    | other ->
      raise (InvalidNumberOfParameters (2, List.length other))
  )
;;

(* Dict access, i.e.: {"foo":1; "bar":[2;3;4]}.{"foo"} *)
_DEFINE_P_FUN "_dict_at"
  (function
    | [DICT d; EXP (String key)] ->
      (
        try 
          let _, value = (List.find (function k, _ -> k = key ) d)
          in value 
        with Not_found ->
          NULL
      )
    | [_; EXP (String _)] ->
      raise (InvalidParameterType (1, "dictionary"))
    | [_; _] ->
      raise (InvalidParameterType (2, "integer"))
    | other ->
      raise (InvalidNumberOfParameters (2, List.length other))
  )
;;


(* *********************************************************** *)
(* ****************** General evaluation ********************* *)
(* *********************************************************** *)


(* 
 * From a list of ocamlets, contained in an expression block in 
 * the input file or string, evaluates them but only returns the last one.
 * This was intended for non-last ocamlets to be used in declarations 
 * or control structures later.
 *)
let eval_expression str =
  let lexbuf = Lexing.from_string str in
  let ocamlets = parse_ocamlet second_level lexbuf in
  let results = rl ocamlets in
  List.hd (List.rev results)
;;
    
(*
 * Printer for type expression
 *)
let rec string_of_expression expression =
  let string_of_expression_list lst =
    "[" ^ (String.concat "; " (List.map string_of_expression lst)) ^ "]" 
  in let string_of_expression_dict d =
    "[" ^
      (String.concat "; " (List.map
        (function st, exp -> st ^ ":" ^ (string_of_expression exp))
        d
      )) ^
    "]"
  in match expression with
  | EXP(Numeric(Int i))         -> string_of_int i
  | EXP(Numeric(Float f))       -> string_of_float f
  | EXP(Boolean b)              -> string_of_bool b
  | EXP(Char c)                 -> string_of_char c
  | EXP(String s)               -> s
  | LIST l                      -> string_of_expression_list l
  | DICT d                      -> string_of_expression_dict d
  | NULL                        -> "null"
;;


let print_ocamlet_evaluation str =
  string_of_expression (eval_expression str)
;;

let peval = print_ocamlet_evaluation;;
