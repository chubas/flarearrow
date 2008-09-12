(* File name *)
let filename = "file_reading.ml";;
(* Open the file *)
let reading_channel = open_in filename;;
(* Create a 2048 character buffer *)
let buf = Buffer.create 2048;;

(* Read line by line of the file in an expandable buffer *)
let read () = 
	let rec loop () = 
	  (* Read a line *)
	  let line = input_line reading_channel in
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

(* Test it *)
print_endline (read ());;