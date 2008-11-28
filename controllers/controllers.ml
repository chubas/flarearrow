(** Main entry for controllers. All controllers should be defined here *)
open Netcgi1_compat.Netcgi_types;;
open Basic_types;;
open Expression_evaluator;;
open Server_utils;;
open Mysql;;
open Flarelib;;

let db_connection = ref None;;
let create_db_connection
  host port user password database =
    Mysql.connect 
    {
      dbhost = host;
      dbport = port;
      dbuser = user;
      dbpwd  = password;
      dbname = database
    }

(* Not sure if this is needed, did this trying to solve the Server_lost (2013)
  error if using multiprocessing instead of multithreading.
*)
let get_db () =
  match !db_connection with
    | None ->
      let conn = create_db_connection 
        Configuration_parameters.db_host
        Configuration_parameters.db_port
        Configuration_parameters.db_user
        Configuration_parameters.db_pwd
        Configuration_parameters.db_dbname
      in
        db_connection := Some conn;
        conn
    | Some connection -> connection
;;

(* Definition section *)
let op_controller_params (cgi:cgi_activation) =
  let op_s = get_variable_or_default cgi "op"   ~default:None in
  let arg1 = get_variable_or_default cgi "arg1" ~default:None in
  let arg2 = get_variable_or_default cgi "arg2" ~default:None in
    let op = match op_s with
      | Some "plus"  -> (+)
      | Some "minus" -> (-)
      | Some "times" -> ( * )
      | Some "div"   -> (/)
      | Some _       -> failwith "Unknown operator"
      | None         -> failwith "Required parameter op"
    in
    match arg1, arg2 with
      | Some a, Some b ->
        ["result", EXP( Numeric (Int (op (int_of_string a) (int_of_string b))))]
      | _, _ -> failwith "Required parameters arg1 and arg2"
;;

let numbers_controller_params (cgi:cgi_activation) =
  let num1 = get_variable_or_default cgi "num1" ~default:None in
  let num2 = get_variable_or_default cgi "num2" ~default:None in
  let rec fact_rec acum n = match n with
    | 1 -> acum
    | e -> fact_rec (acum * n) (n - 1) in
  let fact = fact_rec 1 in
  let sum a = (a * (a + 1)) / 2 in
  let rec gcd a b = match b with
    | 0 -> a
    | _ -> gcd b (a mod b) in
   match num1, num2 with
    | Some a, Some b ->
      let a, b = (int_of_string a), (int_of_string b) in
      let f = fact a in
      let s = sum b in
      let g = gcd a b in
      let env = cgi # environment in
      let fields = env # input_header_fields in
      let concat s (e1, e2) = s ^ e1 ^ ": " ^ e2 ^ "<br />" in  
      let headers = List.fold_left concat "" fields in
      [
        "num1", EXP( Numeric (Int a));
        "num2", EXP( Numeric (Int b));
        "fact", EXP( Numeric (Int f));
        "sum", EXP( Numeric (Int s));
        "gcd", EXP( Numeric (Int g));
        "headers", EXP (String headers)
      ]
    | _, _ -> failwith "Required parameters num1 and num2"
;;

let is_valid_username username =
  let re = Pcre.regexp "^[a-zA-Z0-9_]+$" in
  Pcre.pmatch ~rex:re username
;;

(*** MOCK APPLICATION ***)
let register_controller_params (cgi:cgi_activation) =
  match cgi#request_method with
    | (`GET:request_method) ->
        [
         "start", EXP (Boolean true);
         "errors", LIST [];
         "username", EXP (String "");
         "password", EXP (String "")
        ]
    | `POST ->
		  let _username = get_variable_or_default cgi "username" in
		  let _password = get_variable_or_default cgi "password" in
		  let username = match _username with
		    | None -> ""
		    | Some s -> trim s
		  in
		  let password = match _password with
		    | None -> ""
		    | Some s -> s
		  in
		  let errors = ref [] in
		  let add_error = function e -> errors := e::!errors in
		  let validate_length s desc = 
		    match String.length s  with
		      | 0 -> add_error (desc ^ "cannot be empty")
		      | n when n < 3 -> add_error (desc ^ " must contain at least 3 characters")
		      | n when n > 20 -> add_error (desc ^ " must be less than 20 characters")
		      | _ -> () in
		  List.iter 
		    (function s, desc -> validate_length s desc)
		    [username, "Username"; password, "Password"]; 
		  if not (is_valid_username username) then
		    add_error "Username must only contain numbers, letters and underscores"
		  else ();
		  
		  if !errors = [] then
		    try 
		      Client_lib.insert_user (get_db ()) username password
		    with
		      | Client_lib.DuplicatedUser u -> add_error ("Duplicated username " ^ u);
		  else ();
		     
			[ 
        "start", EXP (Boolean false);
		    "errors", LIST (List.map (function err -> EXP (String err)) !errors);
			  "username", EXP (String username);
			  "password", EXP (String password);
		  ]
   | _ -> failwith "Unrecognized request method"
;;


let handlers = [
	  template_process "hello_world";
	  template_process "oper"
      ~bind_parameters:op_controller_params;
    template_process "headers"
      ~headers:["Content-Type", "text/plain; charset=\"iso-8859-1\""]
      ~path:"/headers_test";
    template_process "numbers"
      ~bind_parameters:numbers_controller_params;
    template_process "register"
      ~bind_parameters:register_controller_params;
];;
