open OUnit;;
open Automata;;

let (==?) = assert_equal;;

let recursion_test_suite = "Automata suite" >:::
  [
    "Test inicio" >:: (fun () ->
      parse "x 1 0 0xF000 lalalala   024 01277l _otro      0xBABEL" ==?
        [
          Identifier "x";
          Decimal "1";
          Decimal "0";
          Hexadecimal "0xF000";
          Identifier "lalalala";
          Octal "024";
          LongOctal "01277l";
          Identifier "_otro";
          LongHexadecimal "0xBABEL"
        ];
      assert_raises NotTerminatedString (fun () ->
        stateA (explode "not terminated") []
      );
      assert_raises _hex_not_terminated (fun () ->
        parse "0x"
      );
      assert_raises (_invalid_character '%') (fun () ->
        parse "fail % here"
      );
    );
  ]
  
let _ =
  run_test_tt ~verbose:true recursion_test_suite;;

let b = "==========================" in
  print_endline b;
  print_endline "No es hermoso OCAML??!!";
  print_endline b;;