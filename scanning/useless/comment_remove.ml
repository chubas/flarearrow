open Exceptions;;
open Flarelib;;

let rec stateA (str:char list) res = match str with
	| h::t -> (
		match h with
			| '{' -> stateB t res
			| '\\'-> stateF t (res ++ h)
			| _ -> stateA t (res ++ h)
		)
	| _ ->  res

and stateB str res = match str with
	| h::t ->(
		match h with
			| '#' -> stateC t (res^"  ")
			| '%' -> stateA t (res^"{%")
			| '{' -> stateG t (res^"{{")
			| _ -> stateA t ((res++'{')++h)
		)
	| _ -> raise NotTerminatedString

and stateC str res = match str with
	| h::t ->(
		match h with
			| '\\' -> stateD t res
			| '#' -> stateE t res
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

and stateF str res= match str with
	| h::t ->(
		match h with
			| _ -> stateA t (res++h)
		)
	| _->raise NotTerminatedString

and stateG str res=match str with
	| h::t->(
		match h with
			| '}'-> stateA t (res++h)
			| _-> stateG t (res++h)
		)
	| _-> raise NotTerminatedString

let rec remove filename =
	let x = file_to_stringf filename in
		let y = string_to_list x in
	stateA y ""
	;;
