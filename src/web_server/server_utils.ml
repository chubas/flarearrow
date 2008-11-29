open Netcgi1_compat.Netcgi_types;;
open Expression_evaluator;;

let template_dir = Configuration_parameters.template_dir;;

let template_process
      ?(bind_parameters = function _ -> [])
      ?(headers = [])
      ?(template_file_function = function (_:string) -> "")
      ?(path:string = "")
      (view:string) = 
  let template_file = match template_file_function view with
    | "" -> view ^ ".fhtml"
    | e  -> e
  in
  let uri_path = if path <> "" then path else (template_dir ^ "/")
  in 
  let process (cgi:cgi_activation) =
    try
      (* Default values for pages *)
      (* Don't do this. A call to cgi#set_header overwrittes previous calls
         until cgi # output # commit_work is called *)
      (*
        cgi # set_header
          ~content_type:"text/html; charset=\"iso-8859-1\""
          ();
      *)
      let contents = parse_file (uri_path ^ template_file) (bind_parameters cgi) in 
      cgi # output # output_string contents;
      (* (headers cgi); *)
      let env = cgi # environment in
        List.iter (function k, v -> env # set_output_header_field k v) headers;
      cgi # output # commit_work ();
    with
        error ->
          cgi # output # rollback_work();
          cgi # set_header
             ~status:        `Internal_server_error
            ~cache:         `No_cache
            ~content_type:  "text/html; charset=\"iso-8859-1\""
            ();
          cgi # output # output_string (Printexc.to_string error);
          cgi # output # commit_work()
  in
    (view, uri_path,
      { Nethttpd_services.dyn_handler = (fun _ -> process );
        dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
        dyn_uri = None;
        dyn_translator = (fun _ -> "");
        dyn_accept_all_conditionals = false;
      }
    )
;;

let get_variable_or_default ?(default = None) (cgi:cgi_activation) var_name =
  try
    Some (cgi # argument_value var_name)
  with
    | Not_found -> default
;;


