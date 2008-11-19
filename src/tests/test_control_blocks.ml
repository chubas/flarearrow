open Position;;
open Grammar_flp;;
open First_level_parser;;
open Basic_types;;
open Expression_evaluator;;
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
        [P_CONTROLFOR ("something ", ["var"], Position (1, 2)); EOF]
        (tfl "{% for var in something %}")
        ~msg:"For test (one variable)";
      assert_equal
        [P_CONTROLFOR ("something ", ["a"; "b"], Position (1, 2)); EOF]
        (tfl "{% for a,b in something %}")
        ~msg:"For test (two variables)";
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
    );
    "Evaluation tests" >:: (fun () ->
      assert_equal 
        "234"
        (parse_string
          "{% for i in [1;2;3]%}{{ i + 1 }}{% endfor %}"
          [])
        ~msg: "Simple for tag (list)";
      assert_equal 
        "one:1two:2"
        (parse_string
          "{% for k, v in {\"one\":1; \"two\":2} %}{{ k }}:{{ v }}{% endfor %}"
          [])
        ~msg: "Simple for tag (dict)";
      assert_equal 
        "H-e-l-l-o-"
        (parse_string
          "{% for chr in \"Hello\" %}{{ chr + \"-\" }}{% endfor %}"
          [])
        ~msg: "Simple for tag (string)";
      assert_equal
        "True!"
        (parse_string
          "{% if true %}True{% endif %}!"
          [])
        ~msg: "Simple if tag";
      assert_equal
        "False!"
        (parse_string
          "{% if false %}True{% else %}False{% endif %}!"
          [])
        ~msg: "Simple if-else tag"
    );
    "Exception tests" >:: (fun () ->
      assert_raises
        Parsing.Parse_error 
        (fun () -> parse_string "{% for i in [1;2;3] %}" [])
        ~msg:"Unfinished for tag";
      assert_raises
        Parsing.Parse_error 
        (fun () -> parse_string "{% if true %}" [])
        ~msg:"Unfinished if tag";
      assert_raises
        (UnrecognizedControlBlock (Position (1, 3))) 
        (fun () -> parse_string "{% badblock %}" [])
        ~msg:"Unrecognized control blocks";
      assert_raises
        (UnrecognizedControlBlock (Position (1, 3))) 
        (fun () -> parse_string "{% for a,b,c in [1;2;3] %}{{ a }}{% endfor %}" [])
        ~msg:"Unrecognized control blocks (number of parameters)";
      assert_raises
        (InvalidNumberOfParameters (1, 2)) 
        (fun () -> parse_string "{% for a, b in [1;2;3] %}{{ a }}{% endfor %}" [])
        ~msg:"Bad parameters for iterator (list)";
      assert_raises
        (InvalidNumberOfParameters (2, 1)) 
        (fun () -> parse_string "{% for a in {\"foo\":\"bar\"} %}{{ a }}{% endfor %}" [])
        ~msg:"Bad parameters for iterator (dict)";
      assert_raises
        (InvalidNumberOfParameters (1, 2)) 
        (fun () -> parse_string "{% for a, b in \"foobar\" %}{{ a }}{% endfor %}" [])
        ~msg:"Bad parameters for iterator (string)";
      assert_raises
        (DuplicatedVariableName "k") 
        (fun () -> 
          parse_string "{% for k in [1;2;3] %}{{ k }}{% endfor %}"
            ["k", EXP (String "duplicated variable")]
        )
        ~msg:"Duplicated (shadowed) variable names";
    );
    "Mixed control blocks tests" >:: ( fun () ->
      assert_equal
        "246"
        (parse_string
          "{% for i in [1;2;3;4;5;6] %}{% if i % 2 == 0 %}{{ i }}{% endif %}{% endfor %}"
          [])
        ~msg: "Nested for-if";
    )
  ];;
  
let test () =  run_test_tt ~verbose:true flp_fixture;;

