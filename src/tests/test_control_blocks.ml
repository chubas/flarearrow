open Position;;
open Grammar_flp;;
open First_level_parser;; 
open OUnit;;

let tfl s = sanitized_first_level (Lexing.from_string s)

let flp_fixture = "First level control blocks suite" >:::
  [
    "Lexing tests" >:: (fun () ->
      assert_equal
        [P_CONTROLIF ("not true ", Position (1, 2)); EOF]
        (tfl "{% if not true %}")
        ~msg:"If test";
      assert_equal
        [P_CONTROLFOR ("something ", Position (1, 2)); EOF]
        (tfl "{% for something %}")
        ~msg:"For test";
      assert_equal
        [P_CONTROLELSE (Position (1, 2)); EOF]
        (tfl "{% else %}")
        ~msg:"Else test";
      assert_equal
        [P_CONTROLENDIF (Position (1, 2)); EOF]
        (tfl "{% endif %}")
        ~msg:"P_CONTROLENDIF test";
      assert_equal
        [P_CONTROLENDFOR (Position (1, 2)); EOF]
        (tfl "{% endfor %}")
        ~msg:"P_CONTROLENDFOR test"
    )
  ];;
  
let test () =  run_test_tt ~verbose:true flp_fixture;;
