open Basic_types;;
open Grammar;;
open Second_level_parser;;

let rec string_of_ocamlet ocamlet = 
  let string_of_ocamlet_list lst =
    "[" ^ (String.concat "; " (List.map string_of_ocamlet lst)) ^ "]" 
  in let string_of_ocamlet_dict d =
    "[" ^
      (String.concat "; " (List.map
        (function st, exp -> st ^ ":" ^ (string_of_ocamlet exp))
        d
      )) ^
    "]"
  in match ocamlet with
  | BASIC(Numeric(Int i))         -> string_of_int i
  | BASIC(Numeric(Float f))       -> string_of_float f
  | BASIC(Boolean b)              -> string_of_bool b
  | BASIC(Char c)                 -> string_of_char c
  | BASIC(String s)               -> s
  | NULL                          -> "null"
  | LIST l                        -> string_of_ocamlet_list l
  | DICT d                        -> string_of_ocamlet_dict d
;;

exception BoundFunctionError;;
exception NotDeclaredFunctionError;;
exception InvalidNumberOfParameters of (int * int);;
exception InvalidParameterType of (int * string);;
exception UnknownException;;

let _FUNCTIONS_HASHTABLE = Hashtbl.create 50;;

let _DEFINE_FUNCTION name funct =
  if Hashtbl.mem _FUNCTIONS_HASHTABLE name then
    raise BoundFunctionError
  else
    Hashtbl.add _FUNCTIONS_HASHTABLE name funct;;

(* ********************** *)
(* Add standard functions *)
(* ********************** *)

_DEFINE_FUNCTION "strlen" 
  (function
    | [BASIC (String s)] -> BASIC (Numeric (Int (String.length s)))
    | [BASIC (Char s)]   -> BASIC (Numeric (Int 1))
    | [_] -> raise (InvalidParameterType (1, "string"))
    | other -> raise (InvalidNumberOfParameters (1, List.length other)) 
  )
;;

(* List or string access, i.e.: "foobar".[1] or [1;2;3].[1] *)
_DEFINE_FUNCTION "_list_at"
  (function
    | [LIST l; BASIC (Numeric (Int i))] ->
      List.nth l i
    | [BASIC (String s); BASIC (Numeric (Int i))] ->
      BASIC (Char s.[i])
    | [_; BASIC (Numeric (Int _))] ->
      raise (InvalidParameterType (1, "list or string"))
    | [_; _] ->
      raise (InvalidParameterType (2, "integer"))
    | other ->
      raise (InvalidNumberOfParameters (2, List.length other))
  )
;;

(* Dict access, i.e.: {"foo":1; "bar":[2;3;4]}.{"foo"} *)
_DEFINE_FUNCTION "_dict_at"
  (function
    | [DICT d; BASIC (String key)] ->
      (
        try 
          let _, value = (List.find (function k, _ -> k = key ) d)
          in value 
        with Not_found ->
          NULL
      )
    | [_; BASIC (String _)] ->
      raise (InvalidParameterType (1, "dictionary"))
    | [_; _] ->
      raise (InvalidParameterType (2, "integer"))
    | other ->
      raise (InvalidNumberOfParameters (2, List.length other))
  )
;;

(* TODO: Add more standard functions *)

let eval_expression str =
  let lexbuf = Lexing.from_string str in
  let ocamlets = parse_ocamlet second_level lexbuf in
  let eval_ocamlet = function
    | FUNCTION (f, params) -> (try 
        (Hashtbl.find _FUNCTIONS_HASHTABLE f) params
      with 
        Not_found -> raise NotDeclaredFunctionError)
    | EXPRESSION exp -> exp in
  let results = List.map eval_ocamlet ocamlets in
  List.hd (List.rev results)
;;

let print_ocaml_evaluation str =
  string_of_ocamlet (eval_expression str)
;;
