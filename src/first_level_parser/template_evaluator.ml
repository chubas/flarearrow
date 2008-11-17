open Position;;
open Grammar_flp;;
open First_level_parser;;
open Flarelib;;

let parse_lexbuf lexbuf =
  let token_list = sanitized_first_level lexbuf in
  (* Hack the way tokens are pushed into a parser and get them from the defined function instead *)
  let pointer = ref 0 in
  let tokenizer_fun (_:Lexing.lexbuf) =
    try
      let return_token = List.nth token_list !pointer in
        pointer := !pointer + 1;
        return_token
    with Failure "nth" -> failwith "Unexpected parsing error"
  in
    parse_template tokenizer_fun lexbuf;;
      
    
  
  let tokens_from_string str = 
    let lexbuf = Lexing.from_string str in
      parse_lexbuf lexbuf;;
  
  let tokens_from_file filename = 
    let str = file_to_string filename in
    tokens_from_string str;;

