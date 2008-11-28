(********************* AUXILIARY FUNCTIONS  *******************)
(* Here until we find or implement better versions *)

let string_to_list string =
  let rec string_to_list_aux string start finish accum =
	  if start <= finish then 
	    string_to_list_aux string (start+1) finish ((String.get string start)::accum)
	  else accum in
  List.rev (string_to_list_aux string 0 ((String.length string)-1) []);;
 
let (++) str char = 
  str ^  (Printf.sprintf "%c" char);;

let string_of_char chr = 
  Printf.sprintf "%c" chr;;

let list_to_string list = 
  List.fold_left (++) "" list;;



(** Read line by line of the file in an expandable buffer and return the file
 contents in a string *)
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

(** Trim a string, i.e. removes whitespaces at the
 beggining and the end *)
let trim str = 
  let rec trim_dir dir s count =
    if s.[count] = ' ' then
      trim_dir dir s (dir count 1)
    else
      count
  in
  let left = trim_dir (+) str 0 in
  let right = trim_dir (-) str (String.length str - 1) in
  let s = String.sub str left ((right - left) + 1) in
    s
;;

(** Shortand for MD5 digest *)
(* Shortand for md5_digest *)
let md5 s = Digest.to_hex (Digest.string s);;
