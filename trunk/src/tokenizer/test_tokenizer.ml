open OUnit
open Tokenizer
open Flarelib

let text_fixture = "block_separator" >:::
  [
    "Single atoms" >:: (fun () ->
      assert_equal
        [Decimal "1"]
        (Tokenizer.tokenize (string_to_list "1"));
      assert_equal
        [Float "1.0"]
        (Tokenizer.tokenize (string_to_list "1.0"));
      assert_equal
        [Float "2.";]
        (Tokenizer.tokenize (string_to_list "2."));
      assert_equal
        [Decimal "1"; Decimal "2"]
        (Tokenizer.tokenize (string_to_list "1 2"));
      assert_equal
        [Float "1234.324"; Float "2134.456"]
        (Tokenizer.tokenize (string_to_list "1234.324 2134.456"));
    );
    "Complex lists" >:: (fun () ->
      assert_equal 
        [
          Decimal "1"; Decimal "0349"; LongDecimal "654L"; LongDecimal "4l";
          Hexadecimal "0xF00D"; LongHexadecimal "0xFFFL"; LongHexadecimal "0xabcl";
          Octal "034"; LongOctal "034L";
          Float "2."; Float "2.0"; Float "4.4"; Float ".5";
          Identifier "wtf"; Identifier "_456"; Identifier "_hey12";
          Str "roger"; Str "stringcito";
          Str "\\"; Str "{{"; Str "}}"; Str "{#"; Str "#}";
        ]
        ( Tokenizer.tokenize
          ( string_to_list
            ( "1 0349 654L 4l " ^
              "0xF00D 0xFFFL 0xabcl " ^
              "034 034L " ^
              "2. 2.0 4.4 .5 " ^
              "wtf _456 _hey12 " ^
              "\"roger\" \"stringcito\" " ^
              "\"\\\" \"{{\" \"}}\" \"{#\" \"#}\""
        )));
      assert_equal
        [Str "\\n";]
        (Tokenizer.tokenize (string_to_list "\"\\n\""));
      );
]

(* Test Runner *)
let _ = run_test_tt ~verbose:true text_fixture;

