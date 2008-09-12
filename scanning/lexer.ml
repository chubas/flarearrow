(* This file represents the first level lexer *)

(* TAGS USED:*)
(* {% Comment inside here %} *)
(* {{ expression }} *)


(* Read the file and put it in a string *)
let buf = Buffer.create 4096
