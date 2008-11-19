{
  open Flarelib;;
  open Position;;
  open Grammar_flp;;

  (*
  type _fpl_token = 
    | P_COMMENT of string * flp_position
    | P_EXPRESSION of string * flp_position
    | P_RAWTEXT of string * flp_position
    | P_CONTROLIF of string * flp_position
    | P_CONTROLFOR of string * (string list) * flp_position
    | P_CONTROLELSE of flp_position
    | P_CONTROLENDIF of flp_position
    | P_CONTROLENDFOR of flp_position
  ;;
*)
  (*
    | Conditional of string * (fpl_token list) * (fpl_token list) * flp_position
    | Iterator of string * (fpl_token list) * fpl_position
  *)

  type fpl_mode = 
    | CommentMode
    | ExpressionMode
    | RawTextMode
    | ControlIfMode | ControlForMode of (string list)
  ;;

  exception NotTerminatedComment of flp_position;;
  exception NotTerminatedExpression of flp_position;;
  exception NotTerminatedString of flp_position;;
  exception InvalidElseTag of flp_position;;
  exception NotTerminatedControlBlock of flp_position;;
  exception UnrecognizedControlBlock of flp_position;;

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
let begin_control = "{%" [' ' '\t']*
let digit = ['0'-'9']
let char_id = ['A'-'Z' 'a'-'z' '_']
let identifier_seq = char_id ( (char_id | digit)* )*

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
            ((P_RAWTEXT (acum, Position mem_pos))::tokens)
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
            ((P_RAWTEXT (acum, Position mem_pos))::tokens)
            "" CommentMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ begin_comment) mode mem_pos lexbuf
    }
    | begin_control "else" [' ' '\t']* "%}" as _s
    {
      match mode with
        | RawTextMode -> 
            let line, col = current_position lexbuf in
            let begin_else = col - (String.length _s) + 2 in
            first_level
              ((P_CONTROLELSE (Position (line, begin_else)))::
                ((P_RAWTEXT (acum, Position mem_pos))::tokens))
              "" RawTextMode (current_position lexbuf) lexbuf
        | _ -> 
          first_level
            tokens (acum ^ _s) mode mem_pos lexbuf
    }
  | begin_control "endif" [' ' '\t']* "%}" as _s
    {
      match mode with
        | RawTextMode -> 
            let line, col = current_position lexbuf in
            let begin_endif = col - (String.length _s) + 2 in
            first_level
              ((P_CONTROLENDIF (Position (line, begin_endif)))::
                ((P_RAWTEXT (acum, Position mem_pos))::tokens))
              "" RawTextMode (current_position lexbuf) lexbuf
        | _ -> 
          first_level
            tokens (acum ^ _s) mode mem_pos lexbuf
    }
  | begin_control "endfor" [' ' '\t']* "%}" as _s
    {
      match mode with
        | RawTextMode -> 
            let line, col = current_position lexbuf in
            let begin_endfor = col - (String.length _s) + 2 in
            first_level
              ((P_CONTROLENDFOR (Position (line, begin_endfor)))::
                ((P_RAWTEXT (acum, Position mem_pos))::tokens))
              "" RawTextMode (current_position lexbuf) lexbuf
        | _ -> 
          first_level
            tokens (acum ^ _s) mode mem_pos lexbuf
    }
  | begin_control "if" [' ' '\t']+ as _s 
    {
      match mode with
        | RawTextMode ->
            let line, col = current_position lexbuf in
            let begin_if = col - (String.length _s) + 2 in
            first_level
              ((P_RAWTEXT (acum, Position mem_pos))::tokens)
              "" ControlIfMode (line, begin_if) lexbuf
        | _ -> 
          first_level
            tokens (acum ^ _s) mode mem_pos lexbuf   
    }
  | (begin_control "for" [' ' '\t']+
    (identifier_seq as first_var) [' ' '\t']* 
    (',' [' ' '\t']* (identifier_seq as second_var))?
    [' ' '\t']* "in" [' ' '\t']* )as _s 
    {
      match mode with
        | RawTextMode ->
            let vars = (match second_var with
              | Some s -> [first_var; s]
              | None -> [first_var]
            ) in
            let line, col = current_position lexbuf in
            let begin_for = col - (String.length _s) + 2 in
            first_level
              ((P_RAWTEXT (acum, Position mem_pos))::tokens)
              "" (ControlForMode vars) (line, begin_for) lexbuf
        | _ -> 
          first_level
            tokens (acum ^ _s) mode mem_pos lexbuf   
    }
  | begin_control     { raise (UnrecognizedControlBlock (Position (current_position lexbuf))) }
  | ("\\#}"|"\\}}"| "\\%}") as escape_end_block
    {
      first_level
        tokens (acum ^ escape_end_block) mode mem_pos lexbuf
    }
  | "}}" as end_expression
    {
      match mode with
        | ExpressionMode ->
          first_level
            ((P_EXPRESSION (acum, Position mem_pos))::tokens)
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
            ((P_COMMENT (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_comment) mode mem_pos lexbuf
    }
  | "%}" as end_control
    {
      
      match mode with
        | ControlIfMode ->
          first_level
            ((P_CONTROLIF (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | ControlForMode vars ->
          first_level
            ((P_CONTROLFOR (acum, vars, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_control) mode mem_pos lexbuf
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
            List.rev (EOF::(P_RAWTEXT (acum, Position mem_pos))::tokens)
          | ExpressionMode -> raise (NotTerminatedExpression (Position mem_pos))
          | CommentMode -> raise (NotTerminatedComment (Position mem_pos))
          | ControlIfMode | (ControlForMode _) -> 
            raise (NotTerminatedControlBlock (Position mem_pos))
      else
        List.rev (EOF::tokens)
    }
and parse_string tokens acum mode mem_pos quote_pos = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      parse_string tokens (acum ^ newline) mode mem_pos quote_pos lexbuf
    }
  | "\\\\" { parse_string tokens (acum ^ "\\") mode mem_pos quote_pos lexbuf }
  | "\\\"" { parse_string tokens (acum ^ "\"") mode mem_pos quote_pos lexbuf }
  | ("}}" | "#}" | "%}") as escaped_symbols
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
  let sanitized_first_level lexbuf = 
    let rec sanitize token_list acum = match token_list with
      | [] -> List.rev acum
      | h::t -> (match h with
        | P_RAWTEXT ("", _) -> sanitize t acum
        | _ -> sanitize t (h::acum)
      )
    in
    sanitize (first_level [] "" RawTextMode (1, 0) lexbuf) []
}
