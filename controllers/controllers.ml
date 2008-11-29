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

let logged_in_user (cgi:cgi_activation) =
  let cookies = cgi # environment # cookies in
  try
    let session_id = List.assoc "flarearrow_session" cookies in
      Some (Client_lib.retrieve_user (get_db ()) session_id)
  with
    | Not_found -> None
;;

let login_controller_params (cgi:cgi_activation) =
  match cgi # request_method with
    | (`GET:request_method) -> 
        [ "start", EXP (Boolean true);
          "error", EXP (Boolean false) ] 
    | (`POST:request_method) ->
      let _username = get_variable_or_default cgi "username" in
      let _password = get_variable_or_default cgi "password" in
      (match (_username, _password) with
        | None, _ | _, None -> failwith "Required username and password"
        | Some username, Some password ->
          ["start", EXP (Boolean false);
           "error", EXP (Boolean 
          ( try
            let db = get_db () in
            ( match logged_in_user cgi with
              (* Cookie is already set *)
              | Some username -> false
              | None -> 
                if Client_lib.user_authenticates db username password then
                  let key = 
                    Client_lib.pwdstring (string_of_float (Unix.time ())) in
                  let cookie = { 
                        cookie_name = "flarearrow_session";
                        cookie_value = key;
                        cookie_expires = None (* At browser exit *);
                        cookie_domain = None;
                    cookie_path = None;
                    cookie_secure = false } in 
                  Client_lib.store_session db key username;
                  cgi # set_header ~set_cookie:[cookie] ();
                  false
                else
                  true
            ) 
          with
            | e -> print_endline (Printexc.to_string e); failwith "Mysql error"
        ))]
      )
    | _ -> failwith "Unrecognized request method"
;;

let logout_controller_params (cgi:cgi_activation) =
  begin 
    match logged_in_user cgi with
      | None -> ()
      | Some username ->
        ignore (
          Mysql.exec (get_db ()) 
            ("delete from sessions where authenticated_user = " ^
            (Mysql.ml2str username) ^ ";")
        );
        let expired_cookie = { 
          cookie_name = "flarearrow_session";
		      cookie_value = "";
		      cookie_expires = Some ((Unix.time ()) -. 10000.); 
		      cookie_domain = None;
          cookie_path = None;
          cookie_secure = false } in 
        cgi # set_header ~set_cookie:[expired_cookie] ();
  end;
  cgi # set_redirection_header "/login";
  []
;;

let random_word () =
  Random.init
    (int_of_float (Unix.time ()));
  List.nth
    Client_lib.word_list
    (Random.int (List.length Client_lib.word_list))
;;

let play_controller_params (cgi:cgi_activation) =
  match logged_in_user cgi with
    | None ->
        cgi # set_redirection_header "/login";
        [
          "username", NULL;
          "random_word", NULL;
          "tries", NULL;
          "corrects", NULL;
          "score", NULL;
        ]
    | Some username ->
        let t, c = Client_lib.user_stats (get_db ()) username in
        let score = match t with
          | 0 -> 0.0  (* Avoid division by zero errors *)
          | _ -> (float_of_int c) /. (float_of_int t)
        in
        [ "username", EXP (String username);
          "random_word",
            EXP(String (random_word ()));
          "tries", EXP(Numeric(Int t));
          "corrects", EXP(Numeric(Int c));
          "score", EXP(Numeric(Float score))
        ]
;;

let guess_controller_params (cgi:cgi_activation) =
  match cgi # request_method with
    | (`POST:request_method) ->
	    (
        match logged_in_user cgi with
	        | None -> 
            cgi # set_redirection_header "/login";
	          ["error", EXP(Boolean true)]
          | Some username ->
              let _guess = get_variable_or_default cgi "guess" in
              let _answer = get_variable_or_default cgi "answer" in
              let is_correct, answer = match (_guess, _answer) with
                | Some g, Some a ->
                  (try
                    ((trim (String.lowercase g)) = (trim (String.lowercase a)), a)
                  with
                    _ -> (false, a)
                  )
                | _ -> failwith "Required parameters 'guess' and 'answer'"
              in 
              let t, c = Client_lib.user_answered (get_db ()) username is_correct in
              let score = match t with
                | 0 -> 0.0  (* Avoid division by zero errors *)
                | _ -> (float_of_int c) /. (float_of_int t)
              in
              [ 
                "error", EXP(Boolean false);
                "was_correct", EXP(Boolean is_correct);
                "score", EXP(Numeric(Float score));
                "tries", EXP(Numeric(Int t));
                "corrects", EXP(Numeric(Int c));
                "correct_word", EXP(String answer);
                "next_word", EXP(String (random_word ()))
              ]
      )
    | _ -> failwith "Invalid request method"
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
    template_process "play"
      ~bind_parameters:play_controller_params;
    template_process "login"
      ~bind_parameters:login_controller_params;
    template_process "logout"
      ~bind_parameters:logout_controller_params;
    template_process "guess"
      ~bind_parameters:guess_controller_params
      ~template_file_function:(function _ -> "result.json")
      ~headers:["Content-type", "text/plain"];
];;

