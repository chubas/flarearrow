{
  type first_level_token = 
    | Comment of string
    | Expression of string
    | RawText of string;;
  
  type mode = 
    | Comment_mode
    | Expression_mode
    | RawText_mode;;

  exception NotTerminatedComment;;
  exception NotTerminatedExpression;;
  exception EOF;;

}

rule first_level tokens acum mode = parse
  | "{{" as begin_expression
    {
      match mode with
        | Expression_mode | Comment_mode ->
          first_level
            tokens (acum ^ begin_expression) mode lexbuf
        | _ ->
          first_level
            ((RawText acum)::tokens) "" Expression_mode lexbuf
    }
  | "{#" as begin_comment
    {
      match mode with
        | Comment_mode | Expression_mode ->
          first_level
            tokens (acum ^ begin_comment) mode lexbuf
        | _ ->
          first_level
            ((RawText acum)::tokens) "" Comment_mode lexbuf
    }
  | ("\\#}"|"\\}}") as escape_end_block
    {
      first_level
        tokens (acum ^ escape_end_block) mode lexbuf
    }
  | "}}" as end_expression
    {
      match mode with
        | Expression_mode ->
          first_level
            ((Expression acum)::tokens) "" RawText_mode lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_expression) mode lexbuf
    }
  | "#}" as end_comment
    {
      match mode with
        | Comment_mode ->
          first_level
            ((Comment acum)::tokens) "" RawText_mode lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_comment) mode lexbuf
    }
  | _ as raw
    {
      first_level
        tokens (acum ^ (Printf.sprintf "%c" raw)) mode lexbuf
    }
  | eof
    {
      if acum <> "" then
        match mode with
          | RawText_mode ->
            List.rev ((RawText acum)::tokens)
          | Expression_mode -> raise NotTerminatedExpression
          | Comment_mode -> raise NotTerminatedComment
      else
        List.rev tokens
    }
{
  
  let rec print_tokens tokens = match tokens with
    | [] -> ()
    | (RawText raw)::t ->
      print_endline ("\x1b[01;32mRaw:\x1b[01;37m " ^ raw);
      print_tokens t
    | (Expression exp)::t ->
      print_endline ("\x1b[01;33mExpression:\x1b[01;37m " ^ exp);
      print_tokens t
    | (Comment com)::t ->
      print_endline ("\x1b[01;34mComment:\x1b[01;37m " ^ com);
      print_tokens t;;
  
  let main () =
  print_endline "==================";
  let cin =
     if Array.length Sys.argv > 1
     then open_in Sys.argv.(1)
     else stdin
    in
      let lexbuf = Lexing.from_channel cin in
        let tokens = first_level [] "" RawText_mode lexbuf in
          print_tokens tokens
          print_enline "\x1b[22;37m";;
  
  let _ = Printexc.print main ()
  
(* To compile:*)
(* 
 * ocamllex -o first_level_lexer.ml first_level_lexer.mll;
 * ocamlc -o first_level_lexer.exe first_level_lexer.ml;
 * ./first_level_lexer.exe [file|stdin];
 *)
}