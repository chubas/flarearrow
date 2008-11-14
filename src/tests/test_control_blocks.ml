open First_level_parser;; 
open OUnit;;

let flp_fixture = "First level control blocks suite" >:::
  [
    "Simple tests" >:: (fun () ->
      assert_equal
        [ControlIf " not true "]
        (tokens_from_string "{% if not true %}")
        ~msg:"If test";
      assert_equal
        [ControlFor " something "]
        (tokens_from_string "{% for something %}")
        ~msg:"For test";
      assert_equal
        [ControlElse]
        (tokens_from_string "{% else %}")
        ~msg:"Else test";
      assert_equal
        [EndIf]
        (tokens_from_string "{% endif %}")
        ~msg:"Endif test";
      assert_equal
        [EndFor]
        (tokens_from_string "{% endfor %}")
        ~msg:"Endfor test"
    )
  ];;
  
let test () =  run_test_tt ~verbose:true flp_fixture;;
