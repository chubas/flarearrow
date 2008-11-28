open Netcgi1_compat.Netcgi_types;;
open Server_config;;
open Server_utils;;
open Controllers;;

let start ?(config_filename = "config.yaml") (_:unit) =
  let start_server () =
    let (opt_list, _) = Netplex_main.args() in
    let cmdline_config = 
      Netplex_main.create 
        ~config_filename:config_filename
        ~pidfile:None
        ~foreground:true () in

    (* For using with MySQL using multi-processing throws errors *)
    let use_mt = ref true in
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
    
    (*
      let config_file, database_connection, template_dir =
          read_config_file config_filename in
      let f = fun _ -> config_file in
    *)
    
    (* TODO: Change configuration here *)
    Server_utils.template_dir := Configuration_parameters.template_dir;

    (* Create connection here *)
    Controllers.db_connection := Some
    (Controllers.create_db_connection 
      Configuration_parameters.db_host
      Configuration_parameters.db_port
      Configuration_parameters.db_user
      Configuration_parameters.db_pwd
      Configuration_parameters.db_dbname);

    let nethttpd_factory =
      let handlers = List.map
        (function view, _, handler -> (view, handler))
        Controllers.handlers in 
      Nethttpd_plex.nethttpd_factory
        ~handlers:handlers
        () in
        
    Netplex_main.startup
      (* ~config_parser:f *)
      parallelizer
      Netplex_log.logger_factories
      Netplex_workload.workload_manager_factories
      [ nethttpd_factory ]
      cmdline_config
  in
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    start_server ()
;;

(* Main start *)
print_endline "Starting server...";
start ~config_filename:"server.conf" ()


