(* This file represents the first level lexer *)

(* TAGS USED:*)
(* {# Comment inside here #} *)
(* {{ expression }} *)

(* Globals *)


(* TODO: Change this to scan incrementally, no need to 
   scan all the file put it in a string, and then scan all the string *)


(* This exception should really never happen.
 * It would only occur when no $ symbol is attached to the end of
 * the string, which method _parse_ does
 *)
(*
let rec reading (str:char list) (result:token list) = 
  match str with h::t -> (
    match h with
      | '{' ->
        bracket_open t result
      | _ ->
        reading t result
    )
  | _ -> raise NotTerminatedString
and bracket_open str result  = 
  match str with h::t -> (
    match h with
      | '{' ->
        expression_start t result
      | '#' ->
        comment_start t result
      | _ ->
        reading t result
    )
  | _ -> raise NotTerminatedString
(* and comment_start str result = *) 


(*let parse str = reading_state (explode (str ^ "$")) [];;*)
*)