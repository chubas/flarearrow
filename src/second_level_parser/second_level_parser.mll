{  
  let (++) str chr = 
    str ^  (Printf.sprintf "%c" chr);;
  
  type slp_numeric =
    | Int of int
    | Float of float
  and slp_basic_type =
    | Numeric of slp_numeric
    | Boolean of bool
    | String of string
    | Char of char
  and slp_operator =
    | PLUS | MINUS | TIMES | DIV | MOD | NEG
    | AND | OR | XOR | NOT
    | EQL | NOT_EQL | GT | LT | GT_EQ | LT_EQ
    | CARET
  and slp_token_p =
    | Identifier of string
    | BasicType of slp_basic_type
    | Operator of slp_operator
    | OPEN_PAR | CLOSE_PAR
    | OPEN_BRK | CLOSE_BRK
    | OPEN_SQRBRK | CLOSE_SQRBRK
    | COLON | SEMICOLON
    | EOF

  type slp_expression =
    | BASIC of slp_basic_type
    | LIST of slp_expression list
    | DICT of (string * slp_expression) list;;

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
  | '"' { BasicType (String (parse_string "" lexbuf)) }
  | char_seq as character
    {
      let chr = match character with
        | "'\\''" -> '\''
        | "'\\\\'" -> '\\'
        | single_char -> single_char.[1] in
      BasicType (Char chr)
    }
  | float_seq   { BasicType (Numeric (Float (float_of_string (Lexing.lexeme lexbuf)))) }
  | int_seq   { BasicType (Numeric (Int (int_of_string (Lexing.lexeme lexbuf)))) }
  | "true"    { BasicType (Boolean true) }
  | "false"   { BasicType (Boolean false) }
  | "+"       { Operator PLUS }
  | "-"       { Operator MINUS }
  | "*"       { Operator TIMES }
  | "/"       { Operator DIV }
  | "%"       { Operator MOD }
  | "~"       { Operator NEG }
  | "and"     { Operator AND }
  | "or"      { Operator OR }
  | "xor"     { Operator XOR }
  | "not"     { Operator NOT }
  | "=="      { Operator EQL }
  | "!="      { Operator NOT_EQL }
  | ">"       { Operator GT }
  | "<"       { Operator LT }
  | ">="      { Operator GT_EQ }
  | "<="      { Operator LT_EQ }
  | "^"       { Operator CARET }
  | ":"       { COLON }
  | ";"       { SEMICOLON }
  | "["       { OPEN_SQRBRK }
  | "]"       { CLOSE_SQRBRK }
  | "("       { OPEN_PAR }
  | ")"       { CLOSE_PAR }
  | "{"       { OPEN_BRK }
  | "}"       { CLOSE_BRK }
  | identifier_seq { Identifier (Lexing.lexeme lexbuf) }
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