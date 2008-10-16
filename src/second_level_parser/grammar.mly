%{

open Basic_types;;

exception BadFunction;;
exception BadArgumentError;;
exception NotImplementedError;;
exception TypeMismatchError;;


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
  | Float a, Float b   -> Float(op_float a b);;

type slp_ocamlet =
  | P_FUN of string * (slp_ocamlet list)
  | P_EXP of slp_expression
and slp_expression =
  | P_BASIC of slp_basic_type
  | P_LIST of slp_ocamlet list
  | P_DICT of (string * slp_ocamlet) list
;;
%}

/*
%token <string * slp_ocamlet list> P_FUN
%token <slp_expression> P_EXP
%token <slp_basic_type> P_BASIC
%token <slp_ocamlet_list> P_LIST
%token <(string * slp_ocamlet) list> P_DICT
*/
%token <Basic_types.slp_numeric> NUMERIC
%token <bool> BOOLEAN
%token <string> STRING
%token <char> CHAR
%token <string> IDENTIFIER
  /* Basic types operators */
%token PLUS MINUS TIMES DIV MOD NEG
%token AND OR XOR NOT
%token EQL NEQL GT LT GT_EQ LT_EQ CARET
  /* Control symbols */
%token OPEN_PAR CLOSE_PAR OPEN_BRK CLOSE_BRK
%token OPEN_SQRBRK CLOSE_SQRBRK
%token COLON SEMICOLON DOT COMMA
%token EOF

  /* Precedence and association */
  /* Lowest to highest */
%left AND OR XOR
%right NOT
%left EQL NEQL GT LT GT_EQ LT_EQ
%left PLUS MINUS
%left TIMES DIV
%left MOD CARET
%right NEG

  /* Entry point */
%start parse_ocamlet
%type <slp_ocamlet list> parse_ocamlet

%%

parse_ocamlet: /* list of slp_ocamlet */
    EOF /* Empty */                               { [] }
  | expression EOF                                { [$1] }
  | expression SEMICOLON SEMICOLON parse_ocamlet  { $1::$4 }
;

expression: /* slp_ocamlet */
    parenthesized_exp             { $1 }
  | funct                         { $1 }
  | basic_exp                     { $1 }
  | elemental_type                { P_EXP $1 }  

parenthesized_exp: /* slp_ocamlet */
    OPEN_PAR expression CLOSE_PAR { $2 }

funct: /* slp_ocamlet */
  | IDENTIFIER OPEN_PAR list_contents CLOSE_PAR     { P_FUN ($1, $3) }
	| single_exp DOT OPEN_SQRBRK expression CLOSE_SQRBRK {
	            P_FUN ("_list_at", [$1; $4]) }
	| single_exp DOT OPEN_BRK expression CLOSE_BRK {
	            P_FUN ("_dict_at", [$1; $4]) }

/* Created to resolve shift/resolve conflict in functions */
/* i.e., there was a shift/reduce conflict in 1 + [2].[3] */
/* to (1 + [2]).[3] and 1 + ([2].[3]) for instance        */
single_exp: /* slp_ocamlet */
    parenthesized_exp               { $1 }
  | funct                           { $1 }
  | elemental_type                  { P_EXP $1 }
                                    
basic_exp: /* slp_ocamlet */
    expression AND   expression   { P_FUN ("_and",   [$1; $3]) }
  | expression OR    expression   { P_FUN ("_or",    [$1; $3]) }
  | expression XOR   expression   { P_FUN ("_xor",   [$1; $3]) }
  | NOT expression                { P_FUN ("_not",   [$2]    ) }
	| expression EQL   expression   { P_FUN ("_eql",   [$1; $3]) }
  | expression NEQL  expression   { P_FUN ("_neql",  [$1; $3]) }
  | expression GT    expression   { P_FUN ("_gt",    [$1; $3]) }
  | expression LT    expression   { P_FUN ("_lt",    [$1; $3]) }
  | expression GT_EQ expression   { P_FUN ("_gt_eq", [$1; $3]) }
  | expression LT_EQ expression   { P_FUN ("_lt_eq", [$1; $3]) }
  | expression PLUS  expression   { P_FUN ("_plus",  [$1; $3]) }
  | expression MINUS expression   { P_FUN ("_minus", [$1; $3]) }
  | expression TIMES expression   { P_FUN ("_times", [$1; $3]) }
	| expression DIV   expression   { P_FUN ("_div",   [$1; $3]) }
  | expression MOD   expression   { P_FUN ("_mod",   [$1; $3]) }
	| expression CARET expression   { P_FUN ("_caret", [$1; $3]) }
  | NEG expression                { P_FUN ("_neg",   [$2])     }
;

elemental_type: /* slp_expression */
	  list                                        { $1 }
	| dict                                        { $1 }
	| basic_type                                  { $1 } 
;

list: /* slp_expression */
  OPEN_SQRBRK list_contents CLOSE_SQRBRK        { P_LIST $2 }
;

list_contents: /* list of slp_ocamlet */ 
	  /* Empty */                                 { [] }
	| expression                                  { [$1] }
	| expression SEMICOLON list_contents          { $1::$3 }
;
  
dict: /* slp_expression */
  OPEN_BRK dict_contents CLOSE_BRK              { P_DICT $2 }
;

dict_contents: /* list of (string * slp_ocamlet ) */
	  /* Empty */                                 { [] }
	| dict_tuple                                  { [$1] }
	| dict_tuple SEMICOLON dict_contents          { $1::$3 }
;
  
dict_tuple: /* (string * slp_expression) */
  string COLON expression                       { $1, $3 }
;

basic_type: /* slp_expression */
    numeric                    { P_BASIC (Numeric $1) }
  | boolean                    { P_BASIC (Boolean $1) }  
  | string                     { P_BASIC (String  $1) }
  | char                       { P_BASIC (Char    $1) }
;

numeric:
  NUMERIC    { $1 }
;
  
boolean:
  BOOLEAN    { $1 }
;

string:
  STRING     { $1 }
;
  
char:
  CHAR       { $1 }
;