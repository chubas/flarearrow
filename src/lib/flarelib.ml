(********************* AUXILIARY FUNCTIONS  *******************)
(* Here until we find or implement better versions *)


let rec string_to_list_aux string start finish accum =
  if start <= finish then 
    string_to_list_aux string (start+1) finish ((String.get string start)::accum)
  else accum;;

let string_to_list string =
  List.rev (string_to_list_aux string 0 ((String.length string)-1) []);;
  
let (++) str char = 
  str ^  (Printf.sprintf "%c" char);;

let string_of_char chr = 
  Printf.sprintf "%c" chr;;

let list_to_string list = 
  List.fold_left (++) "" list;;



(* Read line by line of the file in an expandable buffer and return the file*)
(* contents in a string *)
let file_to_string filename = 
    let channel = open_in filename in
    let buf = Buffer.create 2048 in
    let rec read () = 
      Buffer.add_char buf (input_char channel);
      read ()
    in
    try
      read ()
    with End_of_file -> 
        close_in channel;
        Buffer.contents buf
    ;;