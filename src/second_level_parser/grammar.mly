%{
  
open Basic_types;;

exception BadFunction;;
exception BadArgumentError;;
exception NotImplementedError;;
exception TypeMismatchError;;

let string_of_char c = Printf.sprintf "%c" c;;

(*
 *
 ******* THIS CODE WONT WORK, beacuse OCaml sees the (op a b) in the first match  *******
 ******* and thinks op is of type (int -> int -> 'a), so cannot be applied to     *******
 ******* the following matches. Cookies for the first to hack it and make it work *******
  let compare_comparable x y op = match x, y with
	  | Numeric(Int a), Numeric(Int b)      -> op a b
	  | Numeric(Float a), Numeric(Float b)  -> op a b
		| String a, String b                  -> op a b
		| Char a, Char b                      -> op a b
	  | Numeric(Int a), Numeric(Float b)    ->
	    op (float_of_int a) b
	  | Numeric(Float a), Numeric(Int b)    ->
	    op a (float_of_int b)
		| String a, Char b                    ->
		  op a (string_of_char b)
		| Char a, String b                    ->
		  op (string_of_char a) b
		| _ -> raise TypeMismatchError;
 *****************************************************************************************
*)

(* Coerces basic numeric operations (+ - * /) for ints and floats *)
let numeric_operation x y op_int op_float = match x, y with
  | Int a, Int b       -> Int  (op_int a b)
  | Float a, Int b     -> Float(op_float a (float_of_int b))
  | Int a, Float b     -> Float(op_float (float_of_int a) b)
  | Float a, Float b   -> Float(op_float a b)
;;

type slp_expression =
	| P_BASIC of slp_basic_type
	| P_LIST of slp_ocamlet list
  | P_DICT of (string * slp_ocamlet) list
and slp_ocamlet =
  | P_FUN of string * (slp_ocamlet list)
  | P_EXP of slp_expression
;;

%}

%token <Basic_types.slp_numeric> NUMERIC
%token <bool> BOOLEAN
%token <string> STRING
%token <char> CHAR
%token <string> IDENTIFIER
  /* Basic types operators */
%token PLUS MINUS TIMES DIV MOD NEG
%token AND OR XOR NOT
%token EQL NOT_EQL GT LT GT_EQ LT_EQ CARET
  /* Control symbols */
%token OPEN_PAR CLOSE_PAR OPEN_BRK CLOSE_BRK
%token OPEN_SQRBRK CLOSE_SQRBRK
%token COLON SEMICOLON DOT COMMA
%token EOF
%start parse_ocamlet
%type <slp_ocamlet list> parse_ocamlet

%%

parse_ocamlet: /* list of slp_ocamlet */
    EOF /* Empty */                               { [] }
  | expression EOF                                { [$1] }
  | expression SEMICOLON SEMICOLON parse_ocamlet  { $1::$4 }
;

expression: /* slp_ocamlet */
    OPEN_PAR expression CLOSE_PAR              { $2 }
  | funct                                      { $1 }
  | basic_expression                           { $1 }

basic_expression: /* slp_ocamlet */
  elemental_type                               { P_EXP $1 }
;

funct: /* slp_ocamlet */
	  IDENTIFIER OPEN_PAR list_contents CLOSE_PAR { P_FUN ($1, $3) }
	| expression DOT OPEN_SQRBRK expression CLOSE_SQRBRK {
	            P_FUN ("_list_at", [$1; $4]) }
	| expression DOT OPEN_BRK expression CLOSE_BRK {
	            P_FUN ("_dict_at", [$1; $4]) }
;

elemental_type: /* slp_expression */
	  list                                        { $1 }
	| dict                                        { $1 }
	| basic_type                                  { $1 } 
;

list: /* slp_expression */
  OPEN_SQRBRK list_contents CLOSE_SQRBRK        { P_LIST $2 }
;

dict: /* slp_expression */
  OPEN_BRK dict_contents CLOSE_BRK              { P_DICT $2 }
;

list_contents: /* list of slp_expression */ 
	  /* Empty */                                 { [] }
	| expression                                  { [$1] }
	| expression SEMICOLON list_contents          { $1::$3 }
;
  
dict_contents: /* list of (string * slp_expression ) */
	  /* Empty */                                 { [] }
	| dict_tuple                                  { [$1] }
	| dict_tuple SEMICOLON dict_contents          { $1::$3 }
;
  
dict_tuple: /* (string * slp_expression) */
  string COLON expression                       { $1, $3 }
;
  
basic_type: /* slp_expression */
	  numeric                                     { P_BASIC (Numeric $1) }
	| literal                                     { P_BASIC $1 }
	| boolean                                     { P_BASIC (Boolean $1) } 
;

numeric: /* slp_numeric */
  numeric_times                                 { $1 }
;
  
numeric_times: /* slp_numeric */
	  numeric_times TIMES numeric_plus   { numeric_operation $1 $3 ( * ) ( *. ) }
	| numeric_times DIV numeric_plus     { numeric_operation $1 $3 (/) (/.) }
	| numeric_times MOD numeric_plus     {
	    match $1, $3 with
	      | Int a, Int b ->
	        Int( a mod b )
	      | Float a, Int b ->
	        Int( (int_of_float a) mod b )
	      | Int a, Float b ->
	        Int( a mod (int_of_float b) )
	      | Float a, Float b ->
	        Int( (int_of_float a) mod (int_of_float b) ) }
	| NEG numeric_times {
	    match $2 with
	      | Int a       -> Int( a * -1 )
	      | Float a     -> Float( a *. -1. ) }
	| numeric_plus                        { $1 }
;

numeric_plus: /* slp_numeric */
	  numeric_plus PLUS numeric_atom     { numeric_operation $1 $3 (+) (+.) }
	| numeric_plus MINUS numeric_atom    { numeric_operation $1 $3 (-) (-.) }
	| numeric_atom                       { $1 }
;
  
numeric_atom: /* slp_numeric */
  NUMERIC                              { $1 }
;

boolean: /* boolean */
  boolean_op                           { $1 }
;

boolean_op: /* boolean */
	  boolean_op OR boolean_eq           { $1 || $3 }
	| boolean_op AND boolean_eq          { $1 && $3 }
	| boolean_op XOR boolean_eq          { $1 <> $3 }
	| NOT boolean_op                     { not $2 }
	| boolean_eq                         { $1 }
;

boolean_eq: /* boolean */
	  boolean_eq EQL boolean_comp        { $1 = $3 }
	| boolean_eq NOT_EQL boolean_comp    { $1 <> $3 }
  | boolean_comp                       { $1 }
;

boolean_comp: /* boolean */
  | comparable GT_EQ comparable {
    match $1, $3 with 
		  | Numeric(Int a), Numeric(Int b)      -> a = b
		  | Numeric(Float a), Numeric(Float b)  -> a = b
			| String a, String b                  -> a = b
			| Char a, Char b                      -> a = b
		  | Numeric(Int a), Numeric(Float b)    ->
		    (float_of_int a) = b
		  | Numeric(Float a), Numeric(Int b)    ->
		    a = (float_of_int b)
			| String a, Char b                    ->
			  a = (string_of_char b)
			| Char a, String b                    ->
			  (string_of_char a) = b
			| _ -> raise TypeMismatchError}
	| comparable LT_EQ comparable {
    match $1, $3 with 
		  | Numeric(Int a), Numeric(Int b)      -> a <= b
		  | Numeric(Float a), Numeric(Float b)  -> a <= b
			| String a, String b                  -> a <= b
			| Char a, Char b                      -> a <= b
		  | Numeric(Int a), Numeric(Float b)    ->
		    (float_of_int a) <= b
		  | Numeric(Float a), Numeric(Int b)    ->
		    a <= (float_of_int b)
			| String a, Char b                    ->
			  a <= (string_of_char b)
			| Char a, String b                    ->
			  (string_of_char a) <= b
			| _ -> raise TypeMismatchError}
  | comparable GT comparable    {
    match $1, $3 with 
		  | Numeric(Int a), Numeric(Int b)      -> a >= b
		  | Numeric(Float a), Numeric(Float b)  -> a >= b
			| String a, String b                  -> a >= b
			| Char a, Char b                      -> a >= b
		  | Numeric(Int a), Numeric(Float b)    ->
		    (float_of_int a) >= b
		  | Numeric(Float a), Numeric(Int b)    ->
		    a >= (float_of_int b)
			| String a, Char b                    ->
			  a >= (string_of_char b)
			| Char a, String b                    ->
			  (string_of_char a) >= b
			| _ -> raise TypeMismatchError}
	| comparable LT comparable    {
    match $1, $3 with 
		  | Numeric(Int a), Numeric(Int b)      -> a < b
		  | Numeric(Float a), Numeric(Float b)  -> a < b
			| String a, String b                  -> a < b
			| Char a, Char b                      -> a < b
		  | Numeric(Int a), Numeric(Float b)    ->
		    (float_of_int a) < b
		  | Numeric(Float a), Numeric(Int b)    ->
		    a < (float_of_int b)
			| String a, Char b                    ->
			  a < (string_of_char b)
			| Char a, String b                    ->
			  (string_of_char a) < b
			| _ -> raise TypeMismatchError}
  | boolean_atom                { $1 }

comparable: /* slp_basic_expression */
    numeric                              { Numeric $1 }
  | string                               { String $1 }
  | char                                 { Char $1 }
  | boolean_atom                         { Boolean $1 }
;

boolean_atom: /* boolean */
  BOOLEAN                                { $1 }
;
  
literal: /* slp_basic_type */
	  literal CARET literal_atom {
	    match $1, $3 with
	      | String s, String t ->
	        String( s ^ t )
	      | String s, Char t ->
	        String( s ^ (string_of_char t))
	      | Char s, String t ->
	        String( (string_of_char s) ^ t )
        | _ -> raise TypeMismatchError
	  }
  | literal_atom  { $1 }
;

literal_atom: /* slp_basic_type */
    string                  { String $1 }
  | char                    { Char $1   }
;

string:
  STRING     { $1 }

char:
  CHAR       { $1 }
