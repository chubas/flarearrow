open Parser
open Tokenizer
open Flarelib

let _ =
     try
       let lexbuf = Lexing.from_channel stdin in
       while true do
         let result = Parser.inicio Tokenizer.tokenize (string_to_list lexbuf) in
           print_int result; print_newline(); flush stdout
       done
     with Lexer.Eof ->
       exit 0
