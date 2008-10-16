{
  open Flarelib;;
  
  type flp_position = Position of (int * int);;
  
  type fpl_token = 
    | Comment of string * flp_position
    | Expression of string * flp_position
    | RawText of string * flp_position;;

  type fpl_mode = 
    | CommentMode
    | ExpressionMode
    | RawTextMode;;

  exception NotTerminatedComment of flp_position;;
  exception NotTerminatedExpression of flp_position;;
  exception NotTerminatedString of flp_position;;

  let update_newline lexbuf =    
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- {
	      pos with
	      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	      Lexing.pos_bol = pos.Lexing.pos_cnum
	    };;

  let current_position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
	    let line = pos.Lexing.pos_lnum and
	    col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        (line, col);;

}

let newline_chars = ('\010'|'\013'|"\013\010")

rule first_level tokens acum mode mem_pos = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      first_level tokens (acum ^ newline) mode mem_pos lexbuf
    }
  | '"' as begin_quotes
    {
      match mode with
        | CommentMode | ExpressionMode ->
          parse_string
            tokens (acum ++ begin_quotes) mode mem_pos (current_position lexbuf) lexbuf
        | _ ->
          first_level tokens (acum ++ begin_quotes) mode mem_pos lexbuf
    }
  | "{{" as begin_expression
    {
      match mode with
        | RawTextMode ->
          first_level
            ((RawText (acum, Position mem_pos))::tokens)
            "" ExpressionMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ begin_expression) mode mem_pos lexbuf
    }
  | "{#" as begin_comment
    {
      match mode with
        | RawTextMode ->
          first_level
            ((RawText (acum, Position mem_pos))::tokens)
            "" CommentMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ begin_comment) mode mem_pos lexbuf
    }
  | ("\\#}"|"\\}}") as escape_end_block
    {
      first_level
        tokens (acum ^ escape_end_block) mode mem_pos lexbuf
    }
  | "}}" as end_expression
    {
      match mode with
        | ExpressionMode ->
          first_level
            ((Expression (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_expression) mode mem_pos lexbuf
    }
  | "#}" as end_comment
    {
      match mode with
        | CommentMode ->
          first_level
            ((Comment (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_comment) mode mem_pos lexbuf
    }
  | _ as raw
    {
      first_level
        tokens (acum ++ raw) mode mem_pos lexbuf
    }
  | eof
    {
      if acum <> "" then
        match mode with
          | RawTextMode ->
            List.rev ((RawText (acum, Position mem_pos))::tokens)
          | ExpressionMode -> raise (NotTerminatedExpression (Position mem_pos))
          | CommentMode -> raise (NotTerminatedComment (Position mem_pos))
      else
        List.rev tokens
    }
and parse_string tokens acum mode mem_pos quote_pos = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      parse_string tokens (acum ^ newline) mode mem_pos quote_pos lexbuf
    }
  | "\\\\" { parse_string tokens (acum ^ "\\") mode mem_pos quote_pos lexbuf }
  | "\\\"" { parse_string tokens (acum ^ "\"") mode mem_pos quote_pos lexbuf }
  | ("}}" | "#}") as escaped_symbols
    {
      parse_string tokens (acum ^ escaped_symbols) mode mem_pos quote_pos lexbuf
    }
  | '"' as end_quote
    {
      first_level tokens (acum ++ end_quote) mode mem_pos lexbuf
    }
  | _ as raw
    {
      parse_string tokens (acum ++ raw) mode mem_pos quote_pos lexbuf
    }
  | eof
    {
      raise (NotTerminatedString (Position quote_pos))
    }

{
  let parse_lexbuf lexbuf =
    let rec sanitize token_list acum = match token_list with
      | [] -> List.rev acum
      | h::t -> (match h with
        | RawText ("", _) -> sanitize t acum
        | _ -> sanitize t (h::acum)
      )
    in
    sanitize (first_level [] "" RawTextMode (1, 0) lexbuf) [];;
  
  let tokens_from_string str = 
    let lexbuf = Lexing.from_string str in
      parse_lexbuf lexbuf;;
  
  let tokens_from_file filename = 
    let str = file_to_string filename in
    tokens_from_string str;;
}