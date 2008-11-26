open Flarelib;;
open Yaml_utils;;
open Netplex_types;;
open Mysql;;
open Controllers;;

class address = object end;;
let print_config_tree (config_file:config_file) = 
  let rec pct level node =
    (match node with
      | `Section (_, name, params) ->
        print_endline ((String.make (level * 2) ' ') ^ name ^ ":");
        List.iter (pct (level + 1)) params
      | `Parameter (_, name, value) ->
        let p = (match value with
          | `String s -> s
          | `Int i -> string_of_int i
          | `Float f -> string_of_float f
          | `Bool b -> string_of_bool b
        ) in
        print_endline ((String.make (level * 2) ' ') ^ name ^ " = " ^ p)
    )
  in
    pct 0 (config_file # tree)
;;


let debug = print_endline;;

(**************************************************************************)
(**/**)
(*
 * Following section was almost entirely reused from original Netplex_config
 * definition. Config file has almost the same functionality, but the source
 * code is rewritten and modified here since the source's functionality is
 * hidden by the interface 
 *)
(*
let rec iter_config_tree f count_hash (tree:config_tree) =
  match tree with
    | `Section(addr, name, subtree) ->
      let n = try
        Hashtbl.find count_hash name
      with
        | Not_found ->
          Hashtbl.add count_hash name 0;
          0 in
            Hashtbl.replace count_hash name (n+1);
            f addr name tree;
            List.iter (iter_config_tree f (Hashtbl.create 10)) subtree
    | `Parameter(addr, name, v) ->
      if Hashtbl.mem count_hash name then
        raise(Netplex_config.Config_error("Parameter defined twice: " ^ name));
      Hashtbl.add count_hash name 0;
      f addr name tree;
;;
*)
let rec iter_config_tree f prefix cnt (tree : config_tree) =
  match tree with
    | `Section(addr, name, tl) ->
	let n =
	  try
	    Hashtbl.find cnt name
	  with
	    | Not_found ->
		Hashtbl.add cnt name 0;
		0 in
	Hashtbl.replace cnt name (n+1);
	let fullname =
	  if prefix <> "" then
	    prefix ^ "." ^ name ^ "[" ^ string_of_int n ^ "]"
	  else
	    name in
	f addr fullname tree;
	List.iter (iter_config_tree f fullname (Hashtbl.create 10)) tl
    | `Parameter(addr, name, v) ->
	let fullname = if prefix <> "" then prefix ^ "." ^ name else name in
	if Hashtbl.mem cnt name then
	  raise(Netplex_config.Config_error("Parameter defined twice: " ^ fullname));
	Hashtbl.add cnt name 0;
	f addr fullname tree;
;;


(*
class type flarearrow_config_file = object
  inherit Netplex_types.config_file
  method foo:unit
end
*)

class flarearrow_config_file filename tree :Netplex_types.config_file =
object(self)
  val addresses = Hashtbl.create 100
  initializer(
    debug "Initializing object";
    (*
     (try
      iter_config_tree
        (fun addr name subtree ->
           Hashtbl.add addresses addr (name, subtree))
        (Hashtbl.create 10)
        tree
    with
      | Netplex_config.Config_error msg ->
          raise(Netplex_config.Config_error (filename ^ ": " ^ msg))
    );
    debug ("Params: " ^ (string_of_int (Hashtbl.length addresses)))
    *)
   
		    (try
		      iter_config_tree
					(fun addr fullname subtree ->
					   Hashtbl.add addresses addr (fullname,subtree)
					)
			    ""
			    (Hashtbl.create 10)
			    tree
		    with
		      | Netplex_config.Config_error msg ->
			      raise(Netplex_config.Config_error (filename ^ ": " ^ msg))
		  );
      debug ("Params: " ^ (string_of_int (Hashtbl.length addresses)))

  )

    method filename = filename
    method tree = tree
  
    method root_addr =
      debug "Root addr";
      match tree with
        | `Section(a,_,_) -> a
        | `Parameter(a,_,_) -> a
  
    method root_name =
      match tree with
        | `Section(_,n,_) -> debug ("Root name: " ^ n); n
        | `Parameter(_,n,_) -> debug ("Root name: " ^ n); n
 
  (*
   * Our configuration file is very permissive and
   * does not restrict subsections nor parameters
   *)
  (*
    method restrict_subsections _ lst = 
      debug ("Restricting subsections: " ^ (String.concat "," lst) ^ "--");
      ()
    method restrict_parameters _ lst =
      debug ("Restricting parameters: " ^ (String.concat "," lst) ^ "--");
      ()
      *)
      method restrict_subsections addr names =
    let (fullname, subtree) =
      try
  Hashtbl.find addresses addr
      with
  | Not_found ->
      failwith "#restrict_subsections" in
    match subtree with
      | `Section(_,_,tl) ->
    List.iter
      (function
         | `Section(a,n,_) ->
       if not (List.mem n names) then
         raise(Netplex_config.Config_error(filename ^ ": Section " ^
            self#print addr ^
            " must not contain subsection '" ^
            n ^ "'"))
         | _ -> ())
      tl
      | _ ->
    failwith "#restrict_subsections"

  method restrict_parameters addr names =
    let (fullname, subtree) =
      try
  Hashtbl.find addresses addr
      with
  | Not_found ->
      failwith "#restrict_parameters" in
    match subtree with
      | `Section(_,_,tl) ->
    List.iter
      (function
         | `Parameter(a,n,_) ->
       if not (List.mem n names) then
         raise(Netplex_config.Config_error(filename ^ ": Section " ^
            self#print addr ^
            " must not contain parameter '" ^
            n ^ "'"))
         | _ -> ())
      tl
      | _ ->
    failwith "#restrict_parameters"

      
   
    method resolve_parameter addr name =
      debug ("Resolving parameter " ^ name);
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#resolve_parameter" in
          match subtree with
            | `Section(_,_,tl) ->
              let vl =
                List.map
                (function
                   | `Parameter(addr,_,_) -> addr
                   | _ -> assert false)
                (List.filter
                  (function
                    | `Parameter(_,n,_) -> n = name
                    | _ -> false) tl) in
              (match vl with
                | [] -> raise Not_found
                | [v] -> v
                | _ -> raise(Netplex_config.Config_error(
                  filename ^ ": Multiple definitions for " ^ fullname))
              )
            | `Parameter _ -> raise Not_found
  
    method resolve_section addr name =
      debug ("Resolving section " ^ name);
      let (name, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#resolve_section" in
          match subtree with
            | `Section(_,_,tl) ->
              List.map
              (function
                | `Section(addr,_,_) -> addr
                | _ -> assert false)
              (List.filter
                (function
                  | `Section(_,n,_) -> n = name
                  | _ -> false) tl)
            | `Parameter _ -> []

    method print addr =
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#print" in
          fullname
  
    method string_param addr =
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#string_param" in
          match subtree with
            | `Parameter(_,_,`String s) -> s
            | _ -> raise(Netplex_config.Config_error(
              filename ^ ": Parameter " ^ fullname ^
              " should be a string"))
  
    method int_param addr =
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#int_param" in
          match subtree with
            | `Parameter(_,_,`Int s) -> s
            | _ -> raise(Netplex_config.Config_error(
              filename ^ ": Parameter " ^ fullname ^
              " should be an integer"))
  
    method float_param addr =
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#float_param" in
          match subtree with
            | `Parameter(_,_,`Float s) -> s
            | _ -> raise(Netplex_config.Config_error(
              filename ^ ": Parameter " ^ fullname ^
              " should be a floating-point number"))
  
    method bool_param addr =
      let (fullname, subtree) =
        try Hashtbl.find addresses addr
        with Not_found -> failwith "#bool_param" in
          match subtree with
            | `Parameter(_,_,`Bool b) -> b
            | _ -> raise(Netplex_config.Config_error(
              filename ^ ": Parameter " ^ fullname ^
              " should be a boolean value"))
              
end;;
(**/**)
(************************************)

(** Shortand method for building para config tree, just for convenience *)

(** Build [`Section] *)
let (>>: ) name params : Netplex_types.config_tree = 
  `Section   (new address, name, params);;
(** Build [`Parameter] *)
let (>>>:) name param  : Netplex_types.config_tree =
  `Parameter (new address, name, param );;


(** Auxiliary class for storing all configuration *)
class flarearrow_db_config db_conn =
  object(self)
    method database_connection:Mysql.dbd = db_conn
  end
;;

(* Default params sections *)
let __DEFAULTS = 
  let table = Hashtbl.create 20 in
  let vals = [
    ("bind", `String "{host_address}:{port}");
    ("names", `String "*:0");
    ("static_allow_headers", `String "GET POST");
    ("media_types_file", `String "/etc/mime.types");
    ("max_jobs_per_thread", `Int 1);
    ("min_free_jobs_capacity", `Int 1);
    ("max_free_jobs_capacity", `Int 1);
    ("max_threads", `Int 20);
    ("host_address", `String "0.0.0.0")
  ] in
  List.iter
    (function k, v -> Hashtbl.add table k v)
    vals;
  table
;;

let get_param_or_default table key : Netplex_types.param_value =
  try
    Hashtbl.find table key
  with
    | Not_found -> (
      try
        Hashtbl.find __DEFAULTS key
       with
        | Not_found -> 
            raise (Netplex_config.Config_error 
              ("Required parameter " ^ key)) 
      )

(** Configuration tree template. Just a convenient method for setting
 most common defaults. TODO: Add a very more flexible way to establish
 configuration files and set defaults.
 *)
let config_tree_template
    bind
    names
    static_allow_headers
    media_types_file
    max_jobs_per_thread
    min_free_jobs_capacity
    max_free_jobs_capacity
    max_threads
    host_address
    hostname
    port
    static_doc_root
    (handlers :
      (string * string * 
        Netcgi1_compat.Netcgi_types.cgi_activation
        Nethttpd_services.dynamic_service) list)
    :Netplex_types.config_tree 
  =
  let _port = match port with `Int p -> p | _ -> assert false in
  let _host_address = match host_address with `String h -> h | _ -> assert false in
  let uris:(Netplex_types.config_tree list) =
    List.map
      (function handler, path, _ ->
        "uri" >>: [
          "path" >>>: `String path;
          "service" >>: [
            "type" >>>: `String "dynamic";
            "handler" >>>: `String handler
          ]
        ]
      )
      handlers
  in
  "netplex" >>: [
    "serviceis" >>: [
      "name" >>>: `String "Flarearrow server";
      "protocol" >>: [
        "name" >>>: `String "http";
        "address" >>: [
          "type" >>>: `String "internet";
          "bind" >>>: (match bind with
            | `String b -> let b = 
              (Pcre.replace ~pat:"{host_address}" ~templ:_host_address
                (Pcre.replace ~pat:"{port}" ~templ:(string_of_int _port) b))
              in
                print_endline ("Bind " ^ b);
                `String b
            | _ -> assert false
          )
        ]
      ];
      "processor" >>: [
        "type" >>>: `String "nethttpd";
        "host" >>: [
          "pref_name" >>>: hostname;
          "pref_port" >>>: port;
          "names" >>>: names;
          "uri" >>: [
            "path" >>>: `String "/";
            "method" >>: [
              "allow" >>>: static_allow_headers;
              "service" >>: [
                "type" >>>: `String "file";
                "docroot" >>>: static_doc_root;
                "media_types_file" >>>: media_types_file;
                "enable_listings" >>>: `Bool true
              ]
            ]
          ]
        ] @ uris
      ];
      "workload_manager" >>: [
        "type" >>>: `String "dynamic";
        "max_jobs_per_thread" >>>: max_jobs_per_thread;
        "min_free_jobs_capacity" >>>: min_free_jobs_capacity;
        "max_free_jobs_capacity" >>>: max_free_jobs_capacity;
        "max_threads" >>>: max_threads;
      ]
    ]
  ]
;;

(** Main parsing method. It receives [filename] as the parameter from
 which will read the configuration parameters, formatted in yaml. It will
 then return the configuration file along with the custom parameters
 needed for the framework, such as template directory and database connection
 parameters.
 @param filename The filename that has the configuration file.
*)
let read_config_file filename = 
  try
    let yaml_parser = YamlParser.make () in
    let file_contents = file_to_string filename in
    let root_node = YamlParser.parse_string yaml_parser file_contents in
    match root_node with
      | YamlNode.MAPPING (_, lst) ->
        let hashtable = store_yaml_mapping root_node in
        let get_param p = get_param_or_default hashtable p in
        
        let bind = get_param "bind" in
        let names = get_param "names" in
        let static_allow_headers = get_param "static_allow_headers" in
        let media_types_file = get_param "media_types_file" in
        let max_jobs_per_thread = get_param "max_jobs_per_thread" in
        let min_free_jobs_capacity = get_param "min_free_jobs_capacity" in
        let max_free_jobs_capacity = get_param "max_free_jobs_capacity" in 
        let max_threads = get_param "max_threads" in
        let host_address = get_param "host_address" in
        let hostname = get_param "hostname" in
        let port = get_param "port" in
        let static_doc_root = get_param "static_doc_root" in
        let handlers = Controllers.handlers
        in
        
        let connection =
          let dbhost = get_string_param hashtable "dbhost" in
          let dbname = get_string_param hashtable "dbname" in
          let dbport = get_int_param hashtable "dbport" in
          let dbpwd = get_string_param hashtable "dbpwd" in
          let dbuser = get_string_param hashtable "dbuser" in
              
          (*  TODO  *)
          let db_conn = {
            dbhost = dbhost;
            dbname = dbname;
            dbport = dbport;
            dbpwd  = dbpwd;
            dbuser = dbuser;
          } in 
          new flarearrow_db_config (connect db_conn) in
        
        let template_dir = 
          match get_string_param hashtable "template_dir" with
            | Some s -> s
            | None -> raise 
              (Netplex_config.Config_error "Required parameter template_dir")
        in 
          
        let config_tree = 
          config_tree_template
            bind names static_allow_headers media_types_file
            max_jobs_per_thread min_free_jobs_capacity max_free_jobs_capacity 
            max_threads host_address
            hostname port static_doc_root handlers
        in
        let config_file = new flarearrow_config_file filename config_tree in
        print_config_tree config_file;
        (config_file, connection, template_dir)
      | _ -> raise (Netplex_config.Config_error
               ("Error in parsing file " ^ filename ^
                ": It should contain a YAML mapping as root")) 
  with
    | YamlParser.Error s ->
      raise (Netplex_config.Config_error 
        ("Error in parsing file " ^ filename ^ ":" ^ s))
;; 
