open Netcgi1_compat.Netcgi_types;;
open Yaml_config_parser;;
open Basic_types;;
open Expression_evaluator;;
open Server_utils;;

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

let headers_plain_text (cgi:cgi_activation) =
  cgi # set_header
    ~status: `Accepted
    ~cache:`No_cache
    ~content_type:"text/plain; charset=\"iso-8859-1\""
  ()
;;

Server_utils.start 
  [
	  template_process "hello_world";
	  template_process "oper"
      ~bind_parameters:op_controller_params;
    template_process "headers"
      ~headers:headers_plain_text
  ]
