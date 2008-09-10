let explode str =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (str.[i] :: l) in
  expl (String.length str - 1) [];;

let (++) str chr = str ^  (Printf.sprintf "%c" chr);;

type token =
  | Identifier of string
  | Decimal of string
  | Octal of string
  | Hexadecimal of string
  | LongDecimal of string
  | LongOctal of string
  | LongHexadecimal of string

(* This exception should really never happen.
 * It would only occur when no $ symbol is attached to the end of
 * the string, which method _parse_ does
 *)
exception NotTerminatedString;;

exception SyntaxError of string;;
let _hex_not_terminated = SyntaxError "Hexadecimal int non terminated";;

exception InvalidCharacter of string;;
let _invalid_character c =
  InvalidCharacter (Printf.sprintf "invalid character: %c" c);;

let rec stateA (str:char list) (result:token list) = match str with
  | h::t -> (
    match h with
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '_' ->
        stateJ t result ("" ++ h)
      | '0' ->
        stateB t result ("" ++ h)
      | '1' .. '9' ->
        stateE t result ("" ++ h)
      | ' ' -> 
        stateA t result
      | '$' ->
        List.rev result
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString
and stateB str result partial = match str with
  | h::t -> (
    match h with
      | '0' ->
        stateB t result (partial ++ h)
      | 'x' | 'X' ->
        stateC t result (partial ++ h)
      | '1' .. '7' ->
        stateF t result (partial ++ h)
      | 'l' | 'L' ->
        stateI t result (partial ++ h)
      | ' ' ->
        stateA t ((Decimal partial)::result)
      | '$' -> 
        List.rev ((Decimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateC str result partial = match str with
  | h::t -> (
    match h with
      | 'a' .. 'f'
      | 'A' .. 'F'
      | '0' .. '9' ->
        stateD t result (partial ++ h)
      | ' ' | '$' ->
        raise _hex_not_terminated
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateD str result partial = match str with
  | h::t -> (
    match h with
      | 'a' .. 'f'
      | 'A' .. 'F'
      | '0' .. '9' ->
        stateD t result (partial ++ h)
      | 'l' | 'L' ->
        stateG t result (partial ++ h)
      | ' ' -> 
        stateA t ((Hexadecimal partial)::result)
      | '$' ->
        List.rev ((Hexadecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateE str result partial = match str with
  | h::t -> (
    match h with
      | '0' .. '9' ->
        stateE t result (partial ++ h)
      | ' ' ->
        stateA t ((Decimal partial)::result)
      | '$' ->
        List.rev ((Decimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateF str result partial = match str with
  | h::t -> (
    match h with
      | '0' .. '7' ->
        stateF t result (partial ++ h)
      | 'l' | 'L' ->
        stateH t result (partial ++ h)
      | ' ' ->
        stateA t ((Octal partial)::result)
      | '$' ->
        List.rev ((Octal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateG str result partial = match str with
  | h::t -> (
    match h with
      | ' ' ->
        stateA t ((LongHexadecimal partial)::result)
      | '$' ->
        List.rev ((LongHexadecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateH str result partial = match str with
  | h::t -> (
    match h with
      | ' ' ->
        stateA t ((LongOctal partial)::result)
      | '$' ->
        List.rev ((LongOctal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ ->
    raise NotTerminatedString
    
and stateI str result partial = match str with
  | h::t -> (
    match h with
      | ' ' ->
        stateA t ((LongDecimal partial)::result)
	    | '$' ->
	      List.rev ((LongDecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and stateJ str result partial = match str with
  | h::t -> (
    match h with
	    | 'a' .. 'z'
	    | 'A' .. 'Z'
	    | '0' .. '9'
	    | '_' ->
	      stateJ t result (partial ++ h)
	    | ' ' ->
	      stateA t ((Identifier partial)::result)
	    | '$' ->
	      List.rev ((Identifier partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString
      
      
let parse str = stateA (explode (str ^ "$")) [];;
