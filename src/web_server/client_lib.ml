open Mysql;;
open Flarelib;;

let api_key = "3b6c7c141f55fad34ae21ac70ad3dbbc";;
let secret  = "7689c959aa5eada2";;
let salt    = "w00t";;

let pwdstring s = md5 (s ^ salt);;  

exception UserDoesNotExist of string;;
exception DuplicatedUser of string;;

let check_user_exists db_connection username =
  let result = 
    Mysql.exec db_connection 
    ("select count(*) from users where username = " ^ (Mysql.ml2str username))
  in
    match Mysql.fetch result with
      | None -> assert false
      | Some results -> (match Array.to_list results with
        | [Some "0"] -> false
        | [Some "1"] -> true
        | [Some _  ] -> raise (DuplicatedUser username)
        (* We got more than one user with that name *)
        | _          -> assert false
        (* We got more than a result, which should not occur *)
      ) 
;;

let user_authenticates db_connection username password =
  let result = Mysql.exec db_connection
    ("select * from users where username = " ^ (Mysql.ml2str username) ^ 
     " and password = " ^ (Mysql.ml2str (pwdstring password) ^ ";"))
  in
    match Mysql.size result with
      | 0L -> false
      | 1L -> true
      | _  -> raise (DuplicatedUser username)
;;
  
let user_answered db_connection username is_correct =
  let (current_tries, current_corrects) = 
    let result = Mysql.exec db_connection
      ("select tries, corrects from users where username = " ^ 
        (Mysql.ml2str username)) in
    match fetch result with
      | None -> raise (UserDoesNotExist username)
      | Some results -> (match Array.to_list results with
        | [Some _tries; Some _corrects] -> 
            (int_of_string _tries, int_of_string _corrects)
        | _ -> assert false
      )  
  in
  let new_tries = current_tries + 1 in
  let new_corrects = current_corrects + (if is_correct then 1 else 0) in
  ignore (
    Mysql.exec db_connection
    ("update users set tries = " ^ (Mysql.ml2int new_tries) ^
     ", corrects = " ^ (Mysql.ml2int new_corrects) ^ 
     " where username = " ^ (Mysql.ml2str username)));
    (new_tries, new_corrects)
;;

let print_query db_connection query = 
	let result = Mysql.exec db_connection query in
		Mysql.iter result
		  (fun arr -> 
		    let lst = Array.to_list arr in
		    let m = List.map (function None -> "~" | Some s -> s) lst in
        let pr _ = print_endline "---" in
        pr (); List.iter (print_endline) m; pr ()
	  );;

let insert_user db_connection username password =
  if check_user_exists db_connection username then
    raise (DuplicatedUser username)
  else
	  let query = "insert into users values " ^
	    (Mysql.values
	      [Mysql.ml2str username;
	       Mysql.ml2str (pwdstring password);
	       Mysql.ml2int 0;
	       Mysql.ml2int 0
	      ]) ^
	    ";" in
	  ignore (Mysql.exec db_connection query)
;;