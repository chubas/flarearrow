open Basic_types;; 
open Grammar;;
open Flarelib;;
open Second_level_parser;; 
open First_level_parser;;


(* Reduces type that eliminates functions to basic types *)
type expression =
  | EXP  of slp_basic_type
  | LIST of expression list
  | DICT of (string * expression) list
  | NULL
;;
 
let type_of ?(strict = true) exp = match exp with
  | EXP (Numeric n)         -> if strict then (
    match n with
      | Int _     -> "integer"
      | Float _   -> "float"  
   ) else
    "numeric"
  | EXP (Boolean _)         -> "boolean"
  | EXP string_or_char      -> if strict then (
    match string_or_char with
      | String _  -> "string"
      | Char _    -> "character"
      | _         -> "unknown type" (* Should never get here *)
  ) else
    "string or character"
  | LIST _                  -> "list"
  | DICT _                  -> "dict"
  | NULL                    -> "null"
;;

(*
 * If a dict list contains more than a value for a
 * single key, it just keeps the last one
 *)  
let sanitize_dict list_of_pairs =
  let rec add_or_replace lst acum = match lst with
    | [] -> acum
    | (identifier, value)::tail -> if List.mem_assoc identifier acum then
      add_or_replace tail acum
    else
      add_or_replace tail ((identifier, value)::acum)
  in
    add_or_replace (List.rev list_of_pairs) [] 
;;

exception BoundFunctionError of string;;
exception NotDeclaredFunctionError of string;;

                                  (* expected, given *)
exception InvalidNumberOfParameters of (int * int);;

                                (* position, expected *)
exception InvalidParameterType of (int * string);;

                            (* given, expected type *)
exception BadArgumentError of (expression * string);;
  
                                    (* accesible, given, actual length *)
exception IndexOutOfBoundsException of (expression * int * int);;

                                      (* function, type *)
exception NotSupportedTypeException of string * string;;
exception UnknownException;;
exception UnboundVariable of string;;

(* Hashtable that contains custom function definitions *)
let _P_FUNS_HASHTABLE = Hashtbl.create 50;;

(*
 * Defines a custom function, by associating its name with a function in 
 * the hash table
 * The function should receive as parameter a list of slp_expressions and
 * return an slp_expression
 *)
let _DEFINE_P_FUN (name:string) (funct:(expression list -> expression)) =
  if Hashtbl.mem _P_FUNS_HASHTABLE name then
    raise (BoundFunctionError name)
  else
    Hashtbl.add _P_FUNS_HASHTABLE name funct;;

(*
 * Evals a function by applying the definition stored in the hash table
 * or throwing an exception if the function is not defined
 * The parameters are each evaluated in case any of them is a function itself.
 *)
let rec _EVAL_P_FUN identifier ?(params = []) fun_params = (try
    (Hashtbl.find _P_FUNS_HASHTABLE identifier) (reduce_eval_list fun_params params)
  with 
  | Not_found -> raise (NotDeclaredFunctionError identifier)
)
(* 
 * Transform from slp_ocamlets to expressions, with all
 * P_FUN evaluated and P_VAR bound
 *)
and reduce_eval_list lst params =
  List.map (function
    | P_EXP (P_VAR key)  ->
      (try 
        let value = List.assoc key params in
          value
	    with Not_found ->
        raise (UnboundVariable key)) 
    | P_EXP (P_LIST  l)   -> LIST (reduce_eval_list l params)
    | P_EXP (P_DICT  d)   -> DICT (reduce_eval_dict d params)
    | P_EXP (P_BASIC b)   -> EXP b
    | P_FUN (str, par)    -> _EVAL_P_FUN str par ~params:params
  ) lst

and reduce_eval_dict dict params =
  List.map (function
    | id, P_EXP (P_VAR   k) -> (try 
        let value = List.assoc k params in
          id, value
	    with Not_found ->
        raise (UnboundVariable k)) 
    | id, P_EXP (P_LIST  l) -> id, LIST (reduce_eval_list l params)
    | id, P_EXP (P_DICT  d) -> id, DICT (reduce_eval_dict d params)
    | id, P_EXP (P_BASIC b) -> id, EXP  b
    | id, P_FUN (str,  par) -> id, _EVAL_P_FUN str par ~params:params
  ) (sanitize_dict dict)
;;

(* Alias for reduce_eval_list *)
let rl = reduce_eval_list;;

(* Alias for reduce_eval_dict *)
let rd = reduce_eval_dict;;

(* ********************** *)
(* Add standard functions *)
(* ********************** *)

_DEFINE_P_FUN "_plus"
  (function
    (* Numeric addition *)
    | [EXP (Numeric (Int a)); EXP (Numeric (Int b))] -> 
        EXP (Numeric (Int (a + b)))
    | [EXP (Numeric (Int a)); EXP (Numeric (Float b))]
    | [EXP (Numeric (Float b)); EXP (Numeric (Int a))] ->
        EXP (Numeric (Float (b +. (float_of_int a))))
    | [EXP (Numeric (Float a)); EXP (Numeric (Float b))] -> 
        EXP (Numeric (Float (a +. b)))
    
    (* Literal addition *)
    | [EXP (String s); EXP (String t)] ->
        EXP (String (s ^ t))
    | [EXP (String s); EXP (Char t)] ->
        EXP (String (s ^ (string_of_char t))) 
    | [EXP (Char t); EXP (String s)] ->
        EXP (String ((string_of_char t) ^ s))
    | [EXP (Char s); EXP (Char t)] ->
        EXP (String ((string_of_char s) ^ (string_of_char t)))
   
    (* List addition *)
    | [LIST l; LIST m] ->
        LIST (l @ m)
    
    (* Dict addition *)
    | [DICT d; DICT e] ->
        DICT (sanitize_dict (d @ e))
    
    | [param1; _] -> 
        raise (InvalidParameterType (2, type_of param1 ~strict:false))
    | other ->
        raise (InvalidNumberOfParameters (2, List.length other)) 
  )
;;

_DEFINE_P_FUN "_minus"
  (function
    (* Numeric substraction *)
    | [EXP (Numeric (Int a)); EXP (Numeric (Int b))] -> 
        EXP (Numeric (Int (a - b)))
    | [EXP (Numeric (Int a)); EXP (Numeric (Float b))] ->
        EXP (Numeric (Float ((float_of_int a) -. b)))
    | [EXP (Numeric (Float a)); EXP (Numeric (Int b))] ->
        EXP (Numeric (Float (a -. (float_of_int b))))
    | [EXP (Numeric (Float a)); EXP (Numeric (Float b))] -> 
        EXP (Numeric (Float (a -. b)))    
    | [_; _] -> raise (InvalidParameterType (2, "numeric"))
    | other -> raise (InvalidNumberOfParameters (2, List.length other)) 
  )
;;

_DEFINE_P_FUN "_times"
  (function
    (* Numeric multiplication *)
    | [EXP (Numeric (Int a)); EXP (Numeric (Int b))] -> 
        EXP (Numeric (Int (a * b)))
    | [EXP (Numeric (Int b)); EXP (Numeric (Float a))]
    | [EXP (Numeric (Float a)); EXP (Numeric (Int b))] ->
        EXP (Numeric (Float (a *. (float_of_int b))))
    | [EXP (Numeric (Float a)); EXP (Numeric (Float b))] -> 
        EXP (Numeric (Float (a *. b)))
    (* String * int multiplication *)
    | [EXP (Numeric (Int a)); EXP (String s)]
    | [EXP (String s); EXP (Numeric (Int a))] -> 
	      let rec repeat_string str n acum = match n with
	        | 0 -> acum
	        | n when n > 0 -> repeat_string str (n -1) (acum ^ str)
	        | n -> raise (BadArgumentError ((EXP (Numeric (Int n))), "should not be negative"))
	      in
	       EXP (String (repeat_string s a ""))
    (* Char * int multiplication *)
    | [EXP (Numeric (Int a)); EXP (Char c)]
    | [EXP (Char c); EXP (Numeric (Int a))] ->
        EXP (String (String.make a c))
    (* List * int multiplication *)
    | [EXP (Numeric (Int a)); LIST l]
    | [LIST l; EXP (Numeric (Int a))] ->
        let rec repeat_list lst n acum = match n with
	        | 0 -> acum
	        | n when n > 0 -> repeat_list lst (n -1) (acum @ lst)
	        | n -> raise (BadArgumentError ((EXP (Numeric (Int n))), "should not be negative"))
	      in
	       LIST (repeat_list l a [])
    (* Exceptions with float and other types *)
    | [EXP (Numeric (Float a)); _]
      -> raise (InvalidParameterType (2, "float"))  
    | [_; EXP (Numeric (Float a))] ->
        raise (InvalidParameterType (1, "float"))
    (* Other types *)
    | [param1; _] -> raise (InvalidParameterType (2, type_of param1))
    | other -> raise (InvalidNumberOfParameters (2, List.length other)) 
  )
;;

_DEFINE_P_FUN "_div"
  (function
    (* Numeric division *)
    | [EXP (Numeric (Int a)); EXP (Numeric (Int b))] -> 
        EXP (Numeric (Int (a / b)))
    | [EXP (Numeric (Int a)); EXP (Numeric (Float b))] ->
        EXP (Numeric (Float ((float_of_int a) /. b)))
    | [EXP (Numeric (Float a)); EXP (Numeric (Int b))] ->
        EXP (Numeric (Float (a /. (float_of_int b))))
    | [EXP (Numeric (Float a)); EXP (Numeric (Float b))] -> 
        EXP (Numeric (Float (a /. b)))    
    | [param1; _] -> raise (InvalidParameterType (2, type_of param1))
    | other -> raise (InvalidNumberOfParameters (2, List.length other)) 
  )
;;

_DEFINE_P_FUN "_mod"
  (function
    (* Numeric modulo *)
    | [EXP (Numeric (Int a)); EXP (Numeric (Int b))] -> 
        EXP (Numeric (Int (a mod b)))
    | [EXP (Numeric (Int _)); _] -> 
        raise (InvalidParameterType (2, "integer"))
    | [_; _] -> raise (InvalidParameterType (1, "integer"))
    | other -> raise (InvalidNumberOfParameters (2, List.length other)) 
  )
;;

_DEFINE_P_FUN "_neg"
  (function
    (* Numeric negative *)
    | [EXP (Numeric (Int a))] -> EXP (Numeric (Int (-a)))
    | [EXP (Numeric (Float a))] -> EXP (Numeric (Float (a *. (-1.))))
    | [param1] -> raise (InvalidParameterType (1, "numeric"))
    | other -> raise (InvalidNumberOfParameters (1, List.length other)) 
  )
;;"[1;2] < [1;2]"

(* Curried function generator for comparison expressions *)
let _COMPARE_EXPR operator repr params = match params with
  | [(DICT _) as a; b] | [b; (DICT _) as a] ->
      (match repr with
        | ">" | ">=" | "<" | "<=" ->
          raise (NotSupportedTypeException (repr, "dict"))
        | _ -> EXP (Boolean (operator a b))
      )
  | [NULL as a; b] | [b; NULL as a] ->
      (match repr with
        | ">" | ">=" | "<" | "<=" ->
          raise (NotSupportedTypeException (repr, "null"))
        | _ -> EXP (Boolean (operator a b))
      )
  | [a;b] -> EXP (Boolean (operator a b))
  | other -> ( match List.length other with
    | 2 -> raise (InvalidParameterType (2, type_of (List.nth other 1)))
    | n -> raise (InvalidNumberOfParameters (2, List.length other))
  )
;;

List.iter (function identifier, operator, repr ->
    _DEFINE_P_FUN identifier (_COMPARE_EXPR operator repr)
  )
  [ "_eql", (=), "=="; "_neql", (<>), "!=";
    "_gt", (>), ">"; "_lt", (<), "<";
    "_gt_eq", (>=), ">="; "_lt_eq", (<=), "<="]
;;

(* Allows us to make boolean operations on expressions *)
(* Just null and false values are treated like false   *)
(* MAY CHANGE? *) 
let _BOOL_OF_EXP = function
  | NULL -> false
  | EXP (Boolean b) -> b
  | _ -> true  
;;

(* Curried method for boolean operations *)
let _BOOL_OPERATOR_EXPR operator params = match params with
  | [a;b] -> EXP (Boolean (operator (_BOOL_OF_EXP a) (_BOOL_OF_EXP b)))
  | other -> ( match List.length other with
	| 2 -> raise (InvalidParameterType (2, type_of (List.nth other 1)))
	| n -> raise (InvalidNumberOfParameters (2, List.length other)))
;;

List.iter (function identifier, operator ->
    _DEFINE_P_FUN identifier (_BOOL_OPERATOR_EXPR operator)
  )
  [ "_and", (&&); "_or", (||); "_xor", (<>)]
;;

_DEFINE_P_FUN "_not"
  (function
    | [exp] -> EXP (Boolean (not (_BOOL_OF_EXP exp)))
    | other -> raise (InvalidNumberOfParameters (1, List.length other)) 
  )
;;

_DEFINE_P_FUN "len" 
  (function
    | [EXP (String s)]   -> EXP (Numeric (Int (String.length s)))
    | [EXP (Char   s)]   -> EXP (Numeric (Int 1))
    | [LIST l]           -> EXP (Numeric (Int (List.length l)))
    | [DICT d]           -> EXP (Numeric (Int (List.length d)))
    | [_]       -> raise (InvalidParameterType
                  (1, "string or character or list or dict"))
    | other     -> raise (InvalidNumberOfParameters (1, List.length other)) 
  )
;;

(* List or string access, i.e.: "foobar".[1] or [1;2;3].[1] *)
_DEFINE_P_FUN "_list_at"
  (function
    | [LIST l; EXP (Numeric (Int i))] ->
      (try List.nth l i with 
        | Failure "nth" ->
          raise (IndexOutOfBoundsException (LIST l, i, List.length l)))
    | [EXP (String s); EXP (Numeric (Int i))] ->
      (try EXP (Char s.[i]) with
        | Invalid_argument "index out of bounds" ->
          raise (IndexOutOfBoundsException (EXP (String s), i, String.length s))
      )
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
      raise (InvalidParameterType (2, "string"))
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
let eval_expression str params =
  let lexbuf = Lexing.from_string str in
  let ocamlets = parse_ocamlet second_level lexbuf in
  let results = rl ocamlets params in
  List.hd (List.rev results)
;;

let expand str =
  let lexbuf = Lexing.from_string str in
    parse_ocamlet second_level lexbuf;;
    
(*
 * Printer for type expression
 *)
let rec string_of_expression expression =
  let string_of_expression_list lst =
    "[" ^ (String.concat "; " (List.map string_of_expression lst)) ^ "]" 
  in let string_of_expression_dict d =
    "{" ^
      (String.concat "; " (List.map
        (function st, exp -> st ^ ":" ^ (string_of_expression exp))
        d
      )) ^
    "}"
  in match expression with
  | EXP(Numeric(Int i))         -> string_of_int i
  | EXP(Numeric(Float f))       ->
    (* Add a 0 if the float has no decimal part (e.g. 10. ) *) 
    let fstr = string_of_float f in
      if fstr.[(String.length fstr) - 1] = '.' then
        fstr ^ "0"
      else
        fstr
  | EXP(Boolean b)              -> string_of_bool b
  | EXP(Char c)                 -> string_of_char c
  | EXP(String s)               -> s
  | LIST l                      -> string_of_expression_list l
  | DICT d                      -> string_of_expression_dict d 
  | NULL                        -> "null"
;;


let print_ocamlet_evaluation str params =
  string_of_expression (eval_expression str params)
;;

let peval = print_ocamlet_evaluation;;

let parse_string str params =
	let fl_tokens = tokens_from_string str in
	String.concat "" (
			List.map (function
				| RawText (str, _) -> str
				| Comment (str, _) -> String.make (String.length str) ' '
				| Expression (str, _) -> peval str params
		  ) fl_tokens
		);;

let parse_file file params =
	let fl_tokens = tokens_from_file file in
	String.concat "" (
			List.map (function
				| RawText (str, _) -> str
				| Comment (str, _) -> String.make (String.length str) ' '
				| Expression (str, _) -> peval str params
		  ) fl_tokens
		);;

let peval_no_params str =
  print_ocamlet_evaluation str []
;;


let eval_expression_no_params str =
  eval_expression str []
;;
