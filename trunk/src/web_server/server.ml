open Netcgi1_compat.Netcgi_types;;

let start() =
  let (opt_list, _) = Netplex_main.args() in
  let cmdline_config = 
    Netplex_main.create 
      ~config_filename:"server.config"
      ~pidfile:None
      ~foreground:false () in
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
      ~handlers:[]
      () in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ nethttpd_factory ]
    cmdline_config
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
