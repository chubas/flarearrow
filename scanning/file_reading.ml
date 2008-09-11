(* NOTES: *)
(* This method only works with files, this means channels like sockets and keyboard *)
(* will not work. *)

(* TODO: *)
(* Check the Buffer module and read incrementally, this will help a lot in the*)
(* scanning process *)
(* Unit Tests for File reading !*) 


(* Copies and returns the content in the channel to a string *)
let read_whole_channel channel = 
  (* Get the total of characters using in_channel_length function *)
	let total = in_channel_length channel in
  (* Create a string with total characters length *)
	let result = String.create total in
  (* Put the contents in channel to the result string *)
	really_input channel result 0 total;
	result;;


(* Open the file and put the contents in the channel *)
let read_whole_file filename =
  let channel = open_in filename in
  read_whole_channel channel;;


(* Get the filename and call the reading function *)
let filename = Sys.argv.(1);;
let str = read_whole_file filename;;
Printf.printf "%d characters read from file %s\n" (String.length str) filename