
%{
	(* recordar quitar wrapper *)
 type 'a wrapper =
	| Bool of bool
	| Char of char
	| String of string
	| Int of int
	| List of 'a list
	| Unit of unit
	| Universal of 'a wrapper
	;;

%}

%token <float> FLO
%token <bool> BOOL
%token <string> STR
%token <(unit)list> LIS
%token <bool>UNI
%start inicio
%type <unit> inicio
%%
inicio : exp              { }
    | expb                { }
		| exps                { }
		| expl                { }
		| expu								{ } 
;

exp : exp "*" op          { $1 *. $3 }
    | exp "/" op          { $1 /. $3 }
    | exp "mod" op        { float_of_int(int_of_float $1 mod int_of_float $3) }
    | "-"ter              { -.$2 }
    | op                  { $1 }
;

op : op "+" ter           { $1 +. $3 }
    | op "-" ter          { $1 -. $3 }
    | ter                 { $1 }
;

ter : "(" exp ")"         { $2 }
		| FLO							    { $1 }
;

expb : expb "and" opb     { $1 & $3 }
	| "not" opb             { not $2 }
	| opb                   { $1 }
;

opb : opb "or" terb		    {$1 or $3}
	  | opb "xor" terb		    {$1 <> $3 }
	  | terb							    { $1 }
;

terb : "("expb")"		      { $2 }
	  | BOOL			          { $1 }
;

exps : "string.len" ops 		   { Int (String.length $2) }
	  | "string.get" ops exp     { Char (String.get $2 (int_of_float $3)) }
		| ops                      { String $1 }
;	

ops : ops"^"ters               { $1^$3 }
		| ters                     { $1 }
;
ters : STR			               { $1 }
;

expl : "list.len" opl 		     { Int (List.length $2) }
	  | "list.get" opl exp       { Unit (List.nth $2 (int_of_float $3)) }
		| opl                      { List $1 }
;

opl : "append" opl terl 	     { List.append $2 $3}
;

terl : LIS										 { $1 }
;

expu : expu "=" opu            { $1 = $3 }
    | expu ">=" opu            { $1 >= $3 }
    | expu "<=" opu            { $1 <= $3 }
		| opu                      { $1 }
;

opu : opu "<" teru             { $1 < $3 }
    | opu ">" teru             { $1 > $3 }
		| teru                     { $1 }
;

teru: UNI                      { $1 }
;