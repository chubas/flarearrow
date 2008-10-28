open Flarelib;;
open Netplex_types;;

class address = object end;;

let yaml_parse filename =
  try
    let yaml_parser = YamlParser.make () in
    let file_contents = file_to_string filename in
    YamlParser.parse_string yaml_parser file_contents
  with
    | YamlParser.Error s ->
      print_endline s; raise (YamlParser.Error s)
;; 

let yaml_config_parser filename : Netplex_types.config_file =
let tree:Netplex_types.config_tree =
  `Section (new address, "netplex",
    [(`Parameter (new address, "server_name", `String "flarearrow"))]
  ) in
let config:Netplex_types.config_file =
  object
      method filename = filename
		  method tree = tree
		  method root_addr = object end
		  method root_name = "netplex"
		  method resolve_section = fun _ s ->
        print_endline ("Resolving section " ^ s);
        []
      (*address -> string -> address list*)
		    (* Fails if the address cannot be found. Returns [] if there is no
		     * such section at this address
		     *)
      
		  method resolve_parameter = fun _ s -> 
        print_endline ("Resolving parameter " ^ s);
        new address
      (* address -> string -> address *)
		    (* Fails if the address cannot be found. Raises Not_found if there is no
		     * such parameter at this address
		     *)
		  method print = fun _ -> "1" (* address -> string*)
		  method string_param = fun _ -> "1" (* address -> string *)
		  method int_param = fun _ -> 1 (* address -> int *)
		  method float_param = fun _ -> 1. (* address -> float *)
		  method bool_param = fun _ -> true (* address -> bool *)
		  method restrict_subsections = fun _ _ -> ()
      (* address -> string list -> unit *)
		  method restrict_parameters = fun _ _ -> () 
      (* address -> string list -> unit *)
  end in
  config
;;
