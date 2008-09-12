open Exceptions;;
open Flarelib;;

(* Globals *)
type token =
  | Identifier of string
  | Decimal of string
  | Octal of string
  | Hexadecimal of string
  | LongDecimal of string
  | LongOctal of string
  | LongHexadecimal of string
  | Float of string
  | Str of string
  | Symbol of string


let _hex_not_terminated = SyntaxError "Hexadecimal int non terminated";;
let _invalid_character c =
  InvalidCharacter (Printf.sprintf "invalid character: %c" c);;



(* States *)
let rec reading_state (str:char list) (result:token list) =  
  match str with h::t -> (
    match h with
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '_' ->
        identifier_state t result ("" ++ h)
      | '0' ->
        octal_start_state t result ("" ++ h)
      | '1' .. '9' ->
        decimal_state t result ("" ++ h)
      | ' ' -> 
        reading_state t result
      | '"' ->
        string_state t result "" 
      | '+' | '-' | '/' | '*' ->
        reading_state t (Symbol ("" ++ h) :: result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state result
and identifier_state str result partial =
  match str with h::t -> (
    match h with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '_' ->
          identifier_state t result (partial ++ h)
        | ' ' ->
          reading_state t ((Identifier partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Identifier partial)::result)
and string_state str result partial =
  match str with h::t -> (
    match h with
        | '"' -> reading_state t ((Str partial)::result)
        | '\n' -> raise NotTerminatedString 
        | _ -> string_state t result (partial ++ h)
    )
  | _ -> raise NotTerminatedString

and octal_start_state str result partial = 
  match str with h::t -> (
    match h with
      | '0' ->
        octal_start_state t result (partial ++ h)
      | 'x' | 'X' ->
        hex_start_state t result (partial ++ h)
      | '1' .. '7' ->
        octal_state t result (partial ++ h)
      | 'l' | 'L' ->
        long_decimal_state t result (partial ++ h)
      | ' ' ->
        reading_state t ((Decimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Decimal partial)::result) (* Just a zero was given *)
and hex_start_state str result partial = 
  match str with h::t -> (
    match h with
      | 'a' .. 'f'
      | 'A' .. 'F'
      | '0' .. '9' ->
        hex_state t result (partial ++ h)
      | ' ' ->
        raise _hex_not_terminated
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> raise NotTerminatedString

and hex_state str result partial = 
  match str with h::t -> (
    match h with
      | 'a' .. 'f'
      | 'A' .. 'F'
      | '0' .. '9' ->
        hex_state t result (partial ++ h)
      | 'l' | 'L' ->
        long_hex_state t result (partial ++ h)
      | ' ' -> 
        reading_state t ((Hexadecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Hexadecimal partial)::result)

and decimal_state str result partial = 
  match str with h::t -> (
    match h with
      | '0' .. '9' ->
        decimal_state t result (partial ++ h)
      | ' ' ->
        reading_state t ((Decimal partial)::result)
      | 'l' | 'L' ->
        long_decimal_state t result (partial ++ h)
      | '.' ->
        float_state t result (partial ++ h)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Decimal partial)::result)
and float_state str result partial = 
  match str with h::t -> (
    match h with
      | '0' .. '9' ->
        float_state t result (partial ++ h)
      | ' ' ->
        reading_state t ((Float partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Float partial)::result)
and octal_state str result partial = 
  match str with h::t -> (
    match h with
      | '0' .. '7' ->
        octal_state t result (partial ++ h)
      | 'l' | 'L' ->
        long_octal_state t result (partial ++ h)
      | '8' .. '9' ->
        decimal_state t result (partial ++h)
      | ' ' ->
        reading_state t ((Octal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((Octal partial)::result)

and long_hex_state str result partial = 
  match str with h::t -> (
    match h with
      | ' ' ->
        reading_state t ((LongHexadecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((LongHexadecimal partial)::result)

and long_octal_state str result partial = 
  match str with h::t -> (
    match h with
      | ' ' ->
        reading_state t ((LongOctal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ ->
    end_state ((LongOctal partial)::result)
    
and long_decimal_state str result partial = 
  match str with h::t -> (
    match h with
      | ' ' ->
        reading_state t ((LongDecimal partial)::result)
      | _ ->
        raise (_invalid_character h)
    )
  | _ -> end_state ((LongDecimal partial)::result)
and end_state result = 
  List.rev result

let tokenize string = 
  reading_state string [] 


(* MAIN *)
let list = tokenize (string_to_list "ABCD 0x1234 wawawa 1.234 4345.345 012327");;