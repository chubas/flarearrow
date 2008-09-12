open Exceptions;;
open Flarelib;;

type block =
  | Regular of string
  | Expression of string

(* States *)
let rec regular_state (str:char list) (result:block list) partial =  
  match str with h::t -> (
    match h with
      | '{' ->
        bracket_open t result partial
      | _ -> 
        regular_state t result (partial ++ h) 
  )
  | _ -> end_state ((Regular partial) :: result)
and bracket_open str result partial =
  match str with h::t -> (
    match h with
      | '{' ->
        expression_open t ((Regular partial) :: result) ""
      | _ ->
        regular_state t result (partial ++ '{' ++ h) 
  )
  | _ -> 
    end_state ((Regular partial) :: result)
and expression_open str result partial =
  match str with h::t -> (
    match h with
      | '}' ->
        expression_close t result partial
      | _ ->
        expression_open t result (partial ++ h) 
  )
  | _ -> raise BracketNotClosed
and expression_close str result partial =
  match str with h::t -> (
    match h with 
      | '}' ->
        regular_state t ((Expression partial) :: result) ""
      | _ ->
        expression_open t result (partial ++ '}' ++ h) 
  )
  | _ -> raise BracketNotClosed
and end_state result = 
  List.rev result
  
let separate_blocks string =
  regular_state string [] ""