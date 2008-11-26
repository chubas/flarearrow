open Netplex_types;;

exception InvalidKeyParameter of string;;
                                (* expected received *)
exception InvalidValueParameter of string * string;;

let type_of (param:Netplex_types.param_value) = match param with
  | `String _ -> "String"
  | `Int    _ -> "Integer"
  | `Float  _ -> "Float"
  | `Bool   _ -> "Boolean"
;;

(** Transforms a [YamlNode.t] into 
 corresponding [Netplex_types.param_value] *)
let param_of_node node : Netplex_types.param_value = match node with
  | YamlNode.SCALAR ("str", s) -> `String s
  | YamlNode.SCALAR ("int", i) -> `Int (int_of_string i)
  | YamlNode.SCALAR ("float#fix", f) -> `Float (float_of_string f)
  | YamlNode.SCALAR ("bool#yes", _) -> `Bool true
  | YamlNode.SCALAR ("bool#no", _) -> `Bool false
  | _ -> failwith "#param_of_node"
;;

let store_yaml_mapping yaml_node =
  let store_single_node (k, v) hashtable = match k with
    | YamlNode.SCALAR ("str", strkey) ->
        Hashtbl.replace hashtable strkey (param_of_node v)
    (* TODO: Add other type keys' support? *)
    | _ -> raise (InvalidKeyParameter 
            "Invalid key parameter. Only strings are supported as keys")
  in
  let rec store_yaml_node node hashtable = match node with
    | YamlNode.MAPPING ("", list_of_nodes) ->
        List.iter
        (function
          | (k, ((YamlNode.MAPPING ("", submapping)) as v)) -> 
              store_yaml_node v hashtable
          | pair -> store_single_node pair hashtable)
        list_of_nodes
    | _ -> () (* Ignore this parameter*)
  in 
    let hashtable = Hashtbl.create 10 in
    store_yaml_node yaml_node hashtable;
    hashtable
;;

let get_int_param hashtable name =
  try
    match (Hashtbl.find hashtable name) with
      | `Int i -> Some i
      | other  -> raise (InvalidValueParameter ("Integer", type_of other))
  with
    | Not_found -> None
;;

let get_float_param hashtable name =
  try
    match (Hashtbl.find hashtable name) with
      | `Float f -> Some f
      | other    -> raise (InvalidValueParameter ("Float", type_of other))
  with
    | Not_found -> None
;;

let get_string_param hashtable name =
  try
    match (Hashtbl.find hashtable name) with
      | `String s -> Some s
      | other     -> raise (InvalidValueParameter ("String", type_of other))
  with
    | Not_found -> None
;;

let get_bool_param hashtable name =
  try
    match (Hashtbl.find hashtable name) with
      | `Bool b -> Some b
      | other   -> raise (InvalidValueParameter ("Boolean", type_of other))
  with
    | Not_found -> None
;;