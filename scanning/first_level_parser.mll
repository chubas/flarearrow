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
  
    let tokens_from_file filename = 
    let lexbuf = Lexing.from_channel (open_in filename) in
      first_level [] "" RawText_mode lexbuf;;
  
  let tokens_from_string str = 
    let lexbuf = Lexing.from_string str in
      first_level [] "" RawText_mode lexbuf;;
  
(* To compile:*)
(* 
 * ocamllex -o first_level_lexer.ml first_level_lexer.mll;
 * ocamlc -o first_level_lexer.exe first_level_lexer.ml;
 * ./first_level_lexer.exe [file|stdin];
 *)
}
