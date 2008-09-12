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

(********************* MAIN  *******************) 
let file_contents filename = 
  let reading_channel = open_in filename in
  file_to_string reading_channel;;

 
let filename = Sys.argv.(1);;

let contents = file_contents filename;;
let list = string_to_list contents;;
let list = remove_comments list;;
print_endline (list_to_string list);;