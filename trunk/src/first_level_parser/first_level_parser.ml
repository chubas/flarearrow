# 1 "first_level_parser.mll"
 
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


# 37 "first_level_parser.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\247\255\248\255\001\000\002\000\003\000\001\000\254\255\
    \001\000\255\255\252\255\253\255\004\000\005\000\251\255\250\255\
    \249\255\006\000\007\000\008\000\003\000\002\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\007\000\007\000\007\000\007\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\003\000\003\000\000\000";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \000\000\010\000\255\255\255\255\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\009\000\009\000\009\000\008\000\000\000\000\000\
    \009\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\007\000\003\000\010\000\007\000\013\000\000\000\
    \011\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\011\000\004\000\016\000\015\000\
    \012\000\014\000\014\000\019\000\007\000\007\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\008\000\021\000\000\000\255\255\255\255\
    \017\000\255\255\255\255\017\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\006\000\020\000\005\000\255\255\
    \017\000\017\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\017\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\006\000\000\000\003\000\004\000\
    \005\000\012\000\013\000\017\000\018\000\019\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\017\000\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec first_level tokens acum mode mem_pos lexbuf =
    __ocaml_lex_first_level_rec tokens acum mode mem_pos lexbuf 0
and __ocaml_lex_first_level_rec tokens acum mode mem_pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 39 "first_level_parser.mll"
                     newline
# 143 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 40 "first_level_parser.mll"
    (
      update_newline lexbuf;
      first_level tokens (acum ^ newline) mode mem_pos lexbuf
    )
# 150 "first_level_parser.ml"

  | 1 ->
let
# 44 "first_level_parser.mll"
           begin_quotes
# 156 "first_level_parser.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 45 "first_level_parser.mll"
    (
      match mode with
        | CommentMode | ExpressionMode ->
          parse_string
            tokens (acum ++ begin_quotes) mode mem_pos (current_position lexbuf) lexbuf
        | _ ->
          first_level tokens (acum ++ begin_quotes) mode mem_pos lexbuf
    )
# 167 "first_level_parser.ml"

  | 2 ->
let
# 53 "first_level_parser.mll"
            begin_expression
# 173 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 54 "first_level_parser.mll"
    (
      match mode with
        | RawTextMode ->
          first_level
            ((RawText (acum, Position mem_pos))::tokens)
            "" ExpressionMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ begin_expression) mode mem_pos lexbuf
    )
# 186 "first_level_parser.ml"

  | 3 ->
let
# 64 "first_level_parser.mll"
            begin_comment
# 192 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 65 "first_level_parser.mll"
    (
      match mode with
        | RawTextMode ->
          first_level
            ((RawText (acum, Position mem_pos))::tokens)
            "" CommentMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ begin_comment) mode mem_pos lexbuf
    )
# 205 "first_level_parser.ml"

  | 4 ->
let
# 75 "first_level_parser.mll"
                       escape_end_block
# 211 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 3) in
# 76 "first_level_parser.mll"
    (
      first_level
        tokens (acum ^ escape_end_block) mode mem_pos lexbuf
    )
# 218 "first_level_parser.ml"

  | 5 ->
let
# 80 "first_level_parser.mll"
            end_expression
# 224 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 81 "first_level_parser.mll"
    (
      match mode with
        | ExpressionMode ->
          first_level
            ((Expression (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_expression) mode mem_pos lexbuf
    )
# 237 "first_level_parser.ml"

  | 6 ->
let
# 91 "first_level_parser.mll"
            end_comment
# 243 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 92 "first_level_parser.mll"
    (
      match mode with
        | CommentMode ->
          first_level
            ((Comment (acum, Position mem_pos))::tokens)
            "" RawTextMode (current_position lexbuf) lexbuf
        | _ ->
          first_level
            tokens (acum ^ end_comment) mode mem_pos lexbuf
    )
# 256 "first_level_parser.ml"

  | 7 ->
let
# 102 "first_level_parser.mll"
         raw
# 262 "first_level_parser.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 103 "first_level_parser.mll"
    (
      first_level
        tokens (acum ++ raw) mode mem_pos lexbuf
    )
# 269 "first_level_parser.ml"

  | 8 ->
# 108 "first_level_parser.mll"
    (
      if acum <> "" then
        match mode with
          | RawTextMode ->
            List.rev ((RawText (acum, Position mem_pos))::tokens)
          | ExpressionMode -> raise (NotTerminatedExpression (Position mem_pos))
          | CommentMode -> raise (NotTerminatedComment (Position mem_pos))
      else
        List.rev tokens
    )
# 283 "first_level_parser.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_first_level_rec tokens acum mode mem_pos lexbuf __ocaml_lex_state

and parse_string tokens acum mode mem_pos quote_pos lexbuf =
    __ocaml_lex_parse_string_rec tokens acum mode mem_pos quote_pos lexbuf 17
and __ocaml_lex_parse_string_rec tokens acum mode mem_pos quote_pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 119 "first_level_parser.mll"
                     newline
# 295 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 120 "first_level_parser.mll"
    (
      update_newline lexbuf;
      parse_string tokens (acum ^ newline) mode mem_pos quote_pos lexbuf
    )
# 302 "first_level_parser.ml"

  | 1 ->
let
# 124 "first_level_parser.mll"
                            escaped_symbols
# 308 "first_level_parser.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 125 "first_level_parser.mll"
    (
      parse_string tokens (acum ^ escaped_symbols) mode mem_pos quote_pos lexbuf
    )
# 314 "first_level_parser.ml"

  | 2 ->
let
# 128 "first_level_parser.mll"
           end_quote
# 320 "first_level_parser.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 129 "first_level_parser.mll"
    (
      first_level tokens (acum ++ end_quote) mode mem_pos lexbuf
    )
# 326 "first_level_parser.ml"

  | 3 ->
let
# 132 "first_level_parser.mll"
         raw
# 332 "first_level_parser.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 133 "first_level_parser.mll"
    (
      parse_string tokens (acum ++ raw) mode mem_pos quote_pos lexbuf
    )
# 338 "first_level_parser.ml"

  | 4 ->
# 137 "first_level_parser.mll"
    (
      raise (NotTerminatedString (Position quote_pos))
    )
# 345 "first_level_parser.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_string_rec tokens acum mode mem_pos quote_pos lexbuf __ocaml_lex_state

;;

# 141 "first_level_parser.mll"
 
  let parse_lexbuf lexbuf =
    let rec sanitize token_list acum = match token_list with
      | [] -> List.rev acum
      | h::t -> (match h with
        | RawText ("", _) -> sanitize t acum
        | _ -> sanitize t (h::acum)
      )
    in
    sanitize (first_level [] "" RawTextMode (1, 0) lexbuf) [];;
  
  let tokens_from_file filename = 
    let lexbuf = Lexing.from_channel (open_in filename) in
      parse_lexbuf lexbuf;;
  
  let tokens_from_string str = 
    let lexbuf = Lexing.from_string str in
      parse_lexbuf lexbuf;;


# 372 "first_level_parser.ml"
