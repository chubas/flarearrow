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

Server_utils.start 
  [
	  template_process "hello_world";
	  template_process "oper"
      ~bind_parameters:op_controller_params;
    template_process "headers"
      ~headers:headers_plain_text;
    template_process "numbers"
      ~bind_parameters:numbers_controller_params
  ]
