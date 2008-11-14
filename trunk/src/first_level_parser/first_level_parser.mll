{
  open Flarelib;;
  open Grammar_flp;;
  open Position;;

  (*
  type fpl_mode = 
    | CommentMode
    | ExpressionMode
    | ControlMode
    | RawTextMode
  ;;
*)

  exception UnrecognizedControlBlock of string;;
  exception NotTerminatedString;;

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
let escape_endblocks = ("%}" | "#}" | "}}")

rule first_level acum = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      first_level (acum ^ newline) lexbuf
    }
  | "{{" { Expression (parse_tag "" "}}" lexbuf) }
  | "{#" { Expression (parse_tag "" "#}" lexbuf) }
  | begin_control "if"   { ControlIf (parse_tag "" "%}" lexbuf) }
  | begin_control "else" [' ' '\t']* "%}" { ControlElse }
  | begin_control "endif" [' ' '\t']* "%}" { EndIf }
  | begin_control "endfor" [' ' '\t']* "%}" { EndFor }
  | begin_control _*  { raise (UnrecognizedControlBlock (Lexing.lexeme lexbuf)) }
  | '"' {
      first_level 
        ((acum ++ '"') ^ (parse_string "" lexbuf))
        lexbuf
    }
  | _* as raw { RawText raw }
  | eof      { Eof }
and parse_string acum = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      parse_string (acum ^ newline)  lexbuf
    }
  | "\\\\"   { parse_string (acum ^ "\\") lexbuf }
  | "\\\""   { parse_string (acum ^ "\"") lexbuf }
  | escape_endblocks as e  { parse_string (acum ^ e) lexbuf }
  | '"'      { acum }
  | _ as raw { parse_string (acum ++ raw) lexbuf }    
  | eof      { raise NotTerminatedString }
and parse_tag acum delimiter = parse
  | '"'      { parse_tag (acum ^ (parse_string "" lexbuf)) delimiter lexbuf }
  | escape_endblocks as e { parse_tag (acum ^ e) delimiter lexbuf }
  | ("}}"|"#}"|"%}") as d
    { 
      if d = delimiter then
        acum 
      else 
        parse_tag (acum ^ d) delimiter lexbuf
    }

