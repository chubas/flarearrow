open Netcgi1_compat.Netcgi_types;;
open Yaml_config_parser;;
open Expression_evaluator;;

(* TODO -> Register controllers
let _CONTROLLERS = Hashtbl.create 20;;

let _REGISTER_CONTROLLER str controller =
  Hashtbl.add _CONTROLLERS str controller
;;

let list_of_controller_hash () =
  let lst = ref [] in
    Hashtbl.iter (fun s c -> lst := (s,c)::(!lst) ) _CONTROLLERS;
   lst
;;
*)

let template_process template cgi =
  let template_dir = "/home/chubas/Documents/TEC/7moSemestre/Proyecto/flarearrow/trunk/views/" in
  let processor = try
	    let contents = parse_file (template_dir ^ template) [] in
	      cgi # output # output_string contents;
	      cgi # output # commit_work ()
	  with e ->
	    cgi # output # rollback_work ();
	    cgi # set_header
		    ~status:`Internal_server_error
		    ~cache:`No_cache
		    ~content_type:"text/html; charset=\"iso-8859-1\""
		  ();
	    let error = "Error!" in
	      cgi # output # output_string error;
	      cgi # output # commit_work ()
  in processor
;;

let template_process
      ?(bind_parameters = function _ -> [])
      ?(headers = function (_:cgi_activation) -> ())
      (view:string) = 
  let template = view ^ ".html" in
	let process (cgi:cgi_activation) =
	  try
	    cgi # set_header
	      ~cache:`No_cache
	      ~content_type:"text/html; charset=\"iso-8859-1\""
	      ();
      let template_dir = "/home/chubas/Documents/TEC/7moSemestre/Proyecto/flarearrow/trunk/views/" in
      let contents = parse_file (template_dir ^ template) (bind_parameters cgi) in 
	    cgi # output # output_string contents;
      (headers cgi);
	    cgi # output # commit_work();
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
    (view,
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

let start handlers =
  let start_server () =
	  let (opt_list, _) = Netplex_main.args() in
	  let cmdline_config = 
	    Netplex_main.create 
	      ~config_filename:"server.config"
	      ~pidfile:None
	      ~foreground:true () in
	  let use_mt = ref false in
	  let opt_list' =
	    [ "-mt", Arg.Set use_mt,
	      "  Use multi-threading instead of multi-processing"
	    ] @ opt_list in
	  Arg.parse 
	    opt_list'
	    (fun s -> raise (Arg.Bad ("Bad argument: " ^ s)))
	    "Error";
	  let parallelizer = 
	    if !use_mt then
	      Netplex_mt.mt()
	    else
	      Netplex_mp.mp() in 
	  let nethttpd_factory = 
	    Nethttpd_plex.nethttpd_factory
	      ~handlers:handlers
	      () in
	  Netplex_main.startup
	    parallelizer
	    Netplex_log.logger_factories
	    Netplex_workload.workload_manager_factories
	    [ nethttpd_factory ]
	    cmdline_config
  in
	  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    start_server ()
;;
