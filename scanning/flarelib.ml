(********************* AUXILIARY FUNCTIONS  *******************)
(* Here until we find or implement better versions *)


let rec string_to_list_aux string start finish accum =
  if start <= finish then 
    string_to_list_aux string (start+1) finish ((String.get string start)::accum)
  else accum

let string_to_list string =
  List.rev (string_to_list_aux string 0 ((String.length string)-1) [])
  
let (++) str chr = 
  str ^  (Printf.sprintf "%c" chr);;

let list_to_string list = 
  List.fold_left (++) "" list

(* Read line by line of the file in an expandable buffer and return the file*)
(* contents in a string *)
let file_to_string channel =
  let buf = Buffer.create 4096 in
  let rec loop () = 
    (* Read a line *)
    let line = input_line channel in
    (* Put the line in the buffer *)
    Buffer.add_string buf line;
    (* Put the newline character in the buffer *)
    Buffer.add_char buf '\n';
    (* Continue reading *)
    loop ()
    in
    (* Read the file, and return the contents of the buffer as string
       when EOF is reached *)
    try
      loop ()
    with
      (* Return the contents of the buffer as a string *)
      End_of_file -> Buffer.contents buf
;;

let file_to_stringf filename =
		let buf = Buffer.create 2048 in
	let rec print_all_lines in_chan =
		Buffer.add_string buf ((input_line in_chan)^"\n");
		print_all_lines in_chan
	in
	let in_file = open_in filename in
	try
		print_all_lines in_file
	with End_of_file -> 
		close_in in_file;
		Buffer.contents buf
	;;
