open Exceptions;;
open Flarelib;;

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
	let x = file_to_stringf filename in
		let y = string_to_list (x^"$") in
	stateA y ""
	;;
