(* Exceptions *)
exception NotTerminatedString;;
exception SyntaxError

let rec remove_comments_s contents result =
  match contents with h::t -> (
    match h with
      | '{' ->
        bracket_open_s t result
      | _ ->
        remove_comments_s t (h::result);
    )
  | _ -> 
    result
and bracket_open_s contents result =
  match contents with h::t -> (
    match h with
      | '#' -> 
        comment_open_s t result
      | _ ->
        remove_comments_s t ('{'::result)
  )
  | _ -> raise NotTerminatedString
and comment_open_s contents result = 
  match contents with h::t -> (
    match h with
      | '#' -> 
        comment_close_s t result
      | _ ->
        comment_open_s t result
  )
  | _ -> raise NotTerminatedString
and comment_close_s contents result = 
  match contents with h::t -> (
    match h with
      | '}' -> 
        remove_comments_s t result
      | _ ->
        comment_open_s t result
  )
  | _ -> raise NotTerminatedString

let remove_comments contents = 
  let reversed_contents = remove_comments_s contents [] in
  List.rev reversed_contents;;

(********************* AUXILIARY FUNCTIONS  *******************)
(* to be moved to a better place *)

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



(********************* MAIN  *******************) 
let file_contents filename = 
  let reading_channel = open_in filename in
  file_to_string reading_channel;;

 
let filename = Sys.argv.(1);;

let contents = file_contents filename;;
let list = string_to_list contents;;
let list = remove_comments list;;
print_endline (list_to_string list);;