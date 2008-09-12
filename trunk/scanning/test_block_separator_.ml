open OUnit
open Block_separator
open Flarelib

let text_fixture = "block_separator" >:::
[
    "tokenize" >:: (fun () ->
    assert_equal [Regular "ASDF "; Expression "WTF"; Regular " LALALA"] (Block_separator.separate_blocks (string_to_list ("" ^
                 "ASDF {{WTF}} LALALA"))); (* PASS *)
    assert_equal [Regular "Rogelio Sebastian Ramirez Valenzuela"; 
                  Expression "Expresion bonita"; 
                  Regular "Rogelio ";
                  Expression "expresion";
                  Regular " rogelio";
                  ] 
                  (Block_separator.separate_blocks (string_to_list ("" ^
                 "Rogelio Sebastian Ramirez Valenzuela" ^ 
                 "{{Expresion bonita}}" ^ 
                 "Rogelio {{expresion}} rogelio"
    ))); (* PASS *)
    assert_equal [Regular "{ asdf } rog";
                  Expression "e";
                  Regular "lio";
                 ] 
                  (Block_separator.separate_blocks (string_to_list ("" ^
                 "{ asdf } rog{{e}}lio"
                
    ))); (* PASS *)
);
]

(* Test Runner *)
let _ = run_test_tt ~verbose:true text_fixture;