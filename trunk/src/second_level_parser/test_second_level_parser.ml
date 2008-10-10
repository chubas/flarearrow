open Expression_evaluator;;
open OUnit;;

let flp_fixture = "Second level parser suite" >:::
  [
    "Simple tests" >:: (fun () ->
      assert_equal
        (BASIC (Numeric (Int 1)))
        (eval_expression "1")
        ~msg: "Numeric parse - int";
      assert_equal
        (BASIC (Numeric (Float 2.5)))
        (eval_expression "2.5")
        ~msg: "Numeric parse - float";
      assert_equal
        (BASIC (String "A string"))
        (eval_expression "\"A string\"")
        ~msg: "String parse";
      assert_equal
        (BASIC (Char 'c'))
        (eval_expression "'c'")
        ~msg: "Character parse";
      assert_equal
        (BASIC (Boolean true))
        (eval_expression "true")
        ~msg: "Boolean parse"
    );
    "Numeric operation tests" >:: (fun () ->
      assert_equal
        (BASIC (Numeric (Int 3)))
        (eval_expression "1 + 2")
        ~msg: "Integer sum";
      assert_equal
        (BASIC (Numeric (Float 3.0)))
        (eval_expression "1.0 + 2.0")
        ~msg: "Integer sum";
      assert_equal
        (BASIC (Numeric (Int 5)))
        (eval_expression "6 - 1")
        ~msg: "Integer substraction";
      assert_equal
        (BASIC (Numeric (Float 3.0)))
        (eval_expression "5.0 - 2.0")
        ~msg: "Float substraction";
      assert_equal
        (BASIC (Numeric (Int 6)))
        (eval_expression "3 * 2")
        ~msg: "Integer multiplication";
      assert_equal
        (BASIC (Numeric (Float 6.0)))
        (eval_expression "3.0 * 2.0")
        ~msg: "Float multiplication";
      assert_equal
        (BASIC (Numeric (Int 3)))
        (eval_expression "7 / 2")
        ~msg: "Integer division";
      assert_equal
        (BASIC (Numeric (Float 3.5)))
        (eval_expression "7.0 / 2.0")
        ~msg: "Float division"
    );
    "Integer/float coercion" >:: (fun () ->
      assert_equal
        (BASIC (Numeric (Float 3.0)))
        (eval_expression "1.0 + 2")
        ~msg: "Sum coerce";
      assert_equal
        (BASIC (Numeric (Float 3.0)))
        (eval_expression "10 - 7.0")
        ~msg: "Substraction coerce";
      assert_equal
        (BASIC (Numeric (Float 10.0)))
        (eval_expression "5 * 2.0")
        ~msg: "Multiplication coerce";
      assert_equal
        (BASIC (Numeric (Float 5.0)))
        (eval_expression "10 / 2.0")
        ~msg: "Division coerce";  
    );
    "String operations" >:: (fun () ->
      assert_equal
        (BASIC (String "Hello world"))
        (eval_expression "\"Hello \" + \"world\"")
        ~msg: "String concatenation";
      assert_equal
        (BASIC (Char 'h'))
        (eval_expression "\"Char\"[1]")
        ~msg: "String character at position";
      assert_equal
        (BASIC (Numeric (Int 5)))
        (eval_expression "strlen(\"Hello\")")
        ~msg: "String length"
    );
    "Basic lists and dicts" >:: (fun () ->
      assert_equal
        (LIST [])
        (eval_expression "[]")
        ~msg: "Empty list";
      assert_equal
        (LIST [BASIC (Numeric (Int 1)); BASIC (Numeric (Int 2))])
        (eval_expression "[1; 2]")
        ~msg: "Same type list";
      assert_equal
        (LIST [BASIC (Boolean true); BASIC (Char 'e'); BASIC (String "YAY")])
        (eval_expression "[true; 'e'; \"YAY\"]")
        ~msg: "Polymorphic list";
      assert_equal
        (DICT [])
        (eval_expression "{}")
        ~msg: "Empty dictionary";
      assert_equal
        (DICT 
          [
            "red", BASIC (String "rojo");
            "blue", BASIC (String "azul")
          ])
        (eval_expression "{\"red\":\"rojo\"; \"blue\":\"azul\"}")
        ~msg: "Single type dictionary";
      assert_equal
        (DICT
          [
            "web", BASIC (Numeric (Float 2.0));
            "not true", BASIC (Boolean false);
            "the answer", BASIC (Numeric (Int 42))
          ])
        (eval_expression "{\"web\" : 2.0; \"not true\" : false; \"the answer\" : 42}")
        ~msg: "Polymorphic dictionary";
    );
    "Complex lists and dicts" >:: (fun () ->
      assert_equal
        (LIST [ LIST [] ])
        (eval_expression "[[]]")
        ~msg: "List of lists";
      assert_equal
        (LIST [
          BASIC (Boolean true);
          LIST [
            BASIC (Numeric (Int 10));
            BASIC (Char 'a')
          ];
          DICT [
            "PI", BASIC (Numeric (Float 3.14));
            "tools", LIST [
              BASIC (String "Red bull");
              BASIC (String "Vodka")
            ]
          ]
        ])
        (eval_expression
          ("[true; [10; 'a'];" ^
           "{\"PI\":3.14; \"tools\":[\"Red bull\"; \"Vodka\"]}]"))
        ~msg: "Complex list with nested lists and dicts"
    );
    "List operations" >:: (fun () ->
      assert_equal
        (BASIC (Boolean true))
        (eval_expression "[1; true; 2.5][1]")
        ~msg: "List element at position";
      assert_equal
        (BASIC (Numeric (Int 5)))
        (eval_expression "len([1;2;3;4;5])")
        ~msg: "List length";
      assert_equal
        (LIST
          [
            BASIC (String "Monday");
            BASIC (String "Tuesday");
            BASIC (String "Wednesday");
            BASIC (String "Thursday");
            BASIC (String "Friday");
          ])
        (eval_expression (
          "concat(" ^ 
          "[\"Monday\"; \"Tuesday\"; \"Wednesday\"]" ^
          "[\"Thursday\"; \"Friday\"]" ^
          ")"))
        ~msg: "List concatenation"
    )
  ];;
  
let _= run_test_tt ~verbose:true flp_fixture;;
