{ 
  open Grammar;;
  open Basic_types;;
   
  let parse_error s = print_endline s;;
  
  let (++) str chr = 
    str ^  (Printf.sprintf "%c" chr);;  
  
  (*
  type slp_token_p =
    | IDENTIFIER of string
    (* Basic types operators *)
    | PLUS | MINUS | TIMES | DIV | MOD | NEG
    | AND | OR | XOR | NOT
    | EQL | NOT_EQL | GT | LT | GT_EQ | LT_EQ
    | CARET
    (* Control symbols *)
    | OPEN_PAR | CLOSE_PAR
    | OPEN_BRK | CLOSE_BRK
    | OPEN_SQRBRK | CLOSE_SQRBRK
    | COLON | SEMICOLON
    | DOT | COMMA 
    (* Types *)
    | NUMERIC of slp_numeric
    | BOOLEAN of bool
    | STRING of string
    | CHAR of char
    (* EOF *)
    | EOF
	*)

  exception NotTerminatedString;;

  let update_newline lexbuf =    
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- {
	      pos with
	      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	      Lexing.pos_bol = pos.Lexing.pos_cnum
	    }
  ;;

}

let newline_chars = ( '\010' | '\013' | "\013\010" )
let char_seq = ( "'\\''" | "'\\\\" | '\'' _ '\'' )
let digit = ['0'-'9']
let char_id = ['A'-'Z' 'a'-'z' '_']
let int_seq = digit+
let float_seq = digit+ '.' digit+
let identifier_seq = char_id ( (char_id | digit)* )*

rule second_level = parse
  | newline_chars
    {
      update_newline lexbuf;
      second_level lexbuf
    }
  | [' ' '\t' '\r'] { second_level lexbuf } 
  | '"' { STRING (parse_string "" lexbuf) }
  | char_seq as character
    {
      let chr = match character with
        | "'\\''" -> '\''
        | "'\\\\'" -> '\\'
        | single_char -> single_char.[1] in
      CHAR chr
    }
  | float_seq { NUMERIC (Float (float_of_string (Lexing.lexeme lexbuf))) }
  | int_seq   { NUMERIC (Int (int_of_string (Lexing.lexeme lexbuf))) }
  | "true"    { BOOLEAN true }
  | "false"   { BOOLEAN false }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIV }
  | "%"       { MOD }
  | "~"       { NEG }
  | "and"     { AND }
  | "or"      { OR }
  | "xor"     { XOR }
  | "not"     { NOT }
  | "=="      { EQL }
  | "!="      { NOT_EQL }
  | ">"       { GT }
  | "<"       { LT }
  | ">="      { GT_EQ }
  | "<="      { LT_EQ }
  | "^"       { CARET }
  | ":"       { COLON }
  | ";"       { SEMICOLON }
  | "["       { OPEN_SQRBRK }
  | "]"       { CLOSE_SQRBRK }
  | "("       { OPEN_PAR }
  | ")"       { CLOSE_PAR }
  | "{"       { OPEN_BRK }
  | "}"       { CLOSE_BRK }
  | "."       { DOT }
  | ","       { COMMA }
  | identifier_seq { IDENTIFIER (Lexing.lexeme lexbuf) }
  | eof       { EOF }
and parse_string acum = parse
  | newline_chars as newline
    {
      update_newline lexbuf;
      parse_string (acum ^ newline) lexbuf
    }
  | "\\\\" | "\\\"" as escaped_symbols
    {
      parse_string (acum ^ escaped_symbols) lexbuf
    }
  | '"' { acum }
  | _ as raw { parse_string (acum ++ raw) lexbuf }
  | eof { raise NotTerminatedString }