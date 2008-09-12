let catfile filename =
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

let explode str =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (str.[i] :: l) in
  expl (String.length str - 1) [];;

let (++) str chr = str ^ (Printf.sprintf "%c" chr);;

exception NotTerminatedString;;

exception SyntaxError of string;;
let _hex_not_terminated = SyntaxError "Hexadecimal int non terminated";;

exception InvalidCharacter of string;;
let _invalid_character c =
  InvalidCharacter (Printf.sprintf "invalid character: %c" c);;

let rec stateA (str:char list) res = match str with
	| h::t -> (
		match h with
			| '{' -> stateB t res
			| '$' -> res
			| _ -> stateA t (res ++ h)
		)
	| _ ->  raise NotTerminatedString

and stateB str res = match str with
	| h::t ->(
		match h with
			| '#' -> stateC t (res^"  ")
			| '%' -> stateA t (res^"{%")
			| '{' -> stateA t (res^"{{")
			| '$' -> res
			| _ -> stateA t ((res++'{')++h)
		)
	| _ -> raise NotTerminatedString

and stateC str res = match str with
	| h::t ->(
		match h with
			| '\\' -> stateD t res
			| '#' -> stateE t res
			| '$' -> res
			| _ -> stateC t (res++' ')
		)
	| _ -> raise NotTerminatedString

and stateD str res =	match str with
	| h::t ->(
		match h with
			| _ -> stateC t (res^"  ")
		)
	| _ -> raise NotTerminatedString

and stateE str res =	match str with
	| h::t ->(
		match h with
			| '}'-> stateA t (res^"  ")
			| _ -> stateC t (res^"  ")
		)
	| _ -> raise NotTerminatedString


let rec remove filename =
	let x = catfile filename in
		let y = explode (x^"$") in
	stateA y ""
	;;

let z= remove "uno.html";;
print_string z;;