open Flarelib;;
open Basic_types;;
open Expression_evaluator;;
open OUnit;;

let tests = "Expression Evaluator Tests" >:::
  [
    "Simple expression tests" >:: (fun () ->
      assert_equal 
        (EXP (Numeric (Int 1)))
        (eval_expression_no_params "1")
        ~msg: "Numeric expression - int";
      assert_equal 
        (EXP (Numeric (Float 30.)))
        (eval_expression_no_params "30.0")
        ~msg: "Numeric expression - float";
      assert_equal
        (EXP (String "Hello world"))
        (eval_expression_no_params " \"Hello world\" ")
        ~msg: "String expression";
      assert_equal 
        (EXP (Char 'a'))
        (eval_expression_no_params "'a'")
        ~msg: "Char expression";
      assert_equal 
        (EXP (Boolean true))
        (eval_expression_no_params "true")
        ~msg: "Boolean expression";
      assert_equal
        (LIST [EXP (Numeric (Int 1)); EXP (String "one")])
        (eval_expression_no_params "[1;\"one\"]")
        ~msg: "List expression";
      assert_equal
        (DICT
          [ "monday",EXP(Numeric(Int 1));
            "tuesday", EXP(Numeric(Int 2));
            "wednesday", EXP(Numeric(Int 3))
          ])
        (eval_expression_no_params "{\"monday\":1; \"tuesday\":2; \"wednesday\":3}")
        ~msg: "Dict expression"
      );
    "Dict duplicates test" >:: (fun () ->
      assert_equal
        (DICT
          [ "1", EXP(Numeric(Int 1));
            "3", EXP(Numeric(Int 3));
            "2", EXP(Numeric(Int 42))
          ])
        (eval_expression_no_params "{ \"1\":1; \"2\":2; \"3\":3; \"2\":42 }")
        ~msg: "Dict duplicated test"
    );
    "Single print evaluation tests" >:: (fun () ->
      assert_equal
        "12345" (peval_no_params "12345")
        ~msg: "Print numeric - int";
      assert_equal
        "3.1416" (peval_no_params "3.1416")
        ~msg: "Print numeric - float";
      assert_equal
        "a" (peval_no_params "'a'")
        ~msg: "Print character";
      assert_equal
        "Red bull & vodka" (peval_no_params "\"Red bull & vodka\"")
        ~msg: "Print string";
      assert_equal
        "Backslash: \\ & Quotes: \""
        (peval_no_params "\"Backslash: \\\\ & Quotes: \\\"\"")
        ~msg: "Print string (with escaped characters)";
      assert_equal
        "[1; 2.5; true; a; hello]"
        (peval_no_params "[1; 2.5; true; 'a'; \"hello\"]")
        ~msg: "Print list";
      assert_equal
        "{monday:true; tuesday:2}"
        (peval_no_params "{\"monday\":true; \"tuesday\": 2}")
        ~msg: "Print dict"
    );
    "Numeric operations tests" >:: (fun () ->
      assert_equal
        "12" (peval_no_params "10 + 2")
        ~msg: "Numeric addition - int";
      assert_equal
        "3.1" (peval_no_params "2.5 + 0.6")
        ~msg: "Numeric addition - float";
      assert_equal
        "93" (peval_no_params "100 - 7")
        ~msg: "Numeric substraction - int";
      assert_equal
        "3.5" (peval_no_params "5.0 - 1.5")
        ~msg: "Numeric substraction - float";
      assert_equal
        "30" (peval_no_params "6 * 5")
        ~msg: "Numeric multiplication - int";
      assert_equal
        "5.5" (peval_no_params "0.5 * 11.0")
        ~msg: "Numeric multiplication - float";
      assert_equal
        "5" (peval_no_params "10 / 2")
        ~msg: "Numeric division - int";
      assert_equal
        "2.0" (peval_no_params "100.0 / 50.0")
        ~msg: "Numeric division - float";
      assert_equal
        "1" (peval_no_params "10 % 3")
        ~msg: "Numeric modulo";
      assert_raises
        (InvalidParameterType (1, "integer"))
        (fun () -> peval_no_params "10.5 % 3")
        ~msg: "Numeric modulo error (float given, integer required)";
      assert_raises
        (InvalidParameterType (2, "integer"))
        (fun () -> peval_no_params "10 % 3.0")
        ~msg: "Numeric modulo error (float given, integer required)";
      assert_equal
        "-1" (peval_no_params "~1")
        ~msg: "Numeric negation - int";
      assert_equal
        "-30.5" (peval_no_params "~30.5")
        ~msg: "Numeric negation - float";
    );
    "Numeric coertion test" >:: (fun () ->
      assert_equal
        "10.5" (peval_no_params "10 + 0.5")
        ~msg: "Addition coertion";
      assert_equal
        "20.5" (peval_no_params "40.5 - 20")
        ~msg: "Substraction coertion";
      assert_equal
        "30.0" (peval_no_params "10.0 * 3")
        ~msg: "Multiplication coertion";
      assert_equal
        "2.0" (peval_no_params "10 / 5.0")
        ~msg: "Division coertion"
    );
    "String and char operations" >:: (fun () ->
      assert_equal
        "HelloWorld" (peval_no_params "\"Hello\" + \"World\"")
        ~msg: "String-string concatenation";
      assert_equal
        "Hello World" (peval_no_params "\"Hello\" + ' ' + \"World\"")
        ~msg: "String-char concatenation";
      assert_equal
        "OK" (peval_no_params "'O' + 'K'")
        ~msg: "Char-char concatenation";
      assert_raises
        (InvalidParameterType (2, "string or character"))
        (fun () -> peval_no_params("\"Hello\" + false"))
        ~msg: "Invalid type for concatenation";
      assert_equal
        "e" (peval_no_params "\"Hello\".[1]")
        ~msg: "String access";
      assert_raises
        (InvalidParameterType (2, "integer"))
        (fun () -> peval_no_params("\"Hello\".['x']"))
        ~msg: "Invalid type for string access";
      assert_raises
        (IndexOutOfBoundsException (EXP (String "Hello"), 10, 5))
        (fun () -> peval_no_params("\"Hello\".[10]"))
        ~msg: "Index out of bounds for string access";
      assert_equal
        "10" (peval_no_params "len(\"1234567890\")")
        ~msg: "String length";
      assert_raises
        (InvalidNumberOfParameters (1, 3))
        (fun () -> peval_no_params("len(\"Foo\"; \"Bar\"; \"Baz\")"))
        ~msg: "Bad len: invalid parameters number";
    );
    "List and dict operations" >:: (fun () ->
      assert_equal
        "200" (peval_no_params "[100;200;300].[1]")
        ~msg: "List access";
      assert_equal
        "[1; 2; 3]" (peval_no_params "[1;2] + [3]")
        ~msg: "List concatenation";
      assert_equal
        "2" (peval_no_params "{\"Mercury\":1; \"Venus\":2; \"Earth\":3}.{\"Venus\"}")
        ~msg: "Dict access";
      assert_equal
        "{The beast:666; The answer:42}"
        (peval_no_params "{\"The answer\":69; \"The beast\":666} + {\"The answer\":42}")
        ~msg: "Dict concatenation";
      assert_equal
        "5" (peval_no_params "len([1;2;3;4;5])")
        ~msg: "List length";
      assert_equal
        "3" (peval_no_params "len({\"a\":1; \"b\":2; \"c\":3})")
        ~msg: "Dict length";
      assert_raises
        (InvalidParameterType (2, "integer"))
        (fun () -> peval_no_params("['x'; 'y'; 'z'].['x']"))
        ~msg: "Bad list access: invalid parameter type";
      assert_raises
        (IndexOutOfBoundsException
          (LIST [EXP (Boolean true); EXP (Boolean false)],
          10, 2))
        (fun () -> peval_no_params("[true; false].[10]"))
        ~msg: "Index out of bounds for list access";
      assert_raises
        (InvalidParameterType (2, "string"))
        (fun () -> peval_no_params("{\"here\":false; \"there\": true}.{20}"))
        ~msg: "Bad dict access: invalid parameter type"
    );
    "Boolean operations tests" >:: (fun () ->
      assert_equal
        "false" (peval_no_params "true and false")
        ~msg: "AND test";
      assert_equal
        "true" (peval_no_params "false or true")
        ~msg: "OR test";
      assert_equal
        "true" (peval_no_params "true xor false")
        ~msg: "XOR test";
      assert_equal
        "true" (peval_no_params "not false")
        ~msg: "NOT test";
      assert_equal
        "true" (peval_no_params "true xor true or not false")
        ~msg: "Mixed test";
      assert_equal
        "false" (peval_no_params "true and false")
        ~msg: "AND test"
    );
    "Implicit boolean conversion tests" >:: ( fun () ->
      assert_equal
        "true" (peval_no_params "false or 20")
        ~msg: "Numeric coertion";
      assert_equal
        "false" (peval_no_params "not \"A string\"")
        ~msg: "String coertion";
      assert_equal
        "false" (peval_no_params "true and not [false]")
        ~msg: "List coertion";
      assert_equal
        "true" (peval_no_params "not not {}")
        ~msg: "Dict coertion"
    );
    "Equality tests" >:: (fun () ->
      assert_equal
        "true" (peval_no_params "1 == 1")
        ~msg: "Integer equality";
      assert_equal
        "true" (peval_no_params "3.5 == 3.5")
        ~msg: "Float equality";
      assert_equal
        "true" (peval_no_params "'a' == 'a'")
        ~msg: "Char equality";
      assert_equal
        "true" (peval_no_params "\"foo\" == \"foo\"")
        ~msg: "String equality";
      assert_equal
        "true" (peval_no_params "[1;2;3] == [1;2;3]")
        ~msg: "List equality";
      assert_equal
        "true" (peval_no_params "{\"1\":1; \"2\":2} == {\"1\":1; \"2\":2}")
        ~msg: "Dict equality";
      assert_equal
        "true" (peval_no_params "2 != 1")
        ~msg: "Integer inequality";
      assert_equal
        "true" (peval_no_params "3.51 != 3.5")
        ~msg: "Float inequality";
      assert_equal
        "true" (peval_no_params "'A' != 'a'")
        ~msg: "Char inequality";
      assert_equal
        "true" (peval_no_params "\"foo \" != \"foo\"")
        ~msg: "String inequality";
      assert_equal
        "true" (peval_no_params "[1;3;2] != [1;2;3]")
        ~msg: "List inequality";
      (*
       * TODO: A dict should be equal to other dict if it
       * has the same keys and values, doesn't matter the order
       * Possible solutions: order the list in the comparison,
       * make a Set instead a list, use a hashtable from the beggining
       *)
      assert_equal
        "true" (peval_no_params "{\"1\":1; \"2\":2} != {\"1\":1; \"2\":2; \"3\":3}")
        ~msg: "Dict inequality";
    );
    "Comparison tests" >:: (fun () ->
      assert_equal "true" (peval_no_params "not 1 > 10")
        ~msg: "int > int comparison";
      assert_equal "true" (peval_no_params "30.0 > 1.4")
        ~msg: "float > float comparison";
      assert_equal "true" (peval_no_params "\"zebra\" > \"ant\"")
        ~msg: "string > string comparison";
      assert_equal "true" (peval_no_params "'b' > 'a'")
        ~msg: "char > char comparison";
      assert_equal "true" (peval_no_params "[1;2] > [1;1;2]")
        ~msg: "list > list comparison";
      assert_raises
        (NotSupportedTypeException (">", "dict"))
        (fun () -> peval_no_params "{\"2\":2} > {\"1\":1}")
        ~msg: "Bad comparison: dict > dict";
        
      assert_equal "true" (peval_no_params "1 < 10")
        ~msg: "int < int comparison";
      assert_equal "false" (peval_no_params "10.0 < 3.5")
        ~msg: "float < float comparison";
      assert_equal "true" (peval_no_params "\"zebra\" < \"zebras\"")
        ~msg: "string < string comparison";
      assert_equal "true" (peval_no_params "'b' > 'a'")
        ~msg: "char < char comparison";
      assert_equal "true" (peval_no_params "[1] < [1;2]")
        ~msg: "list < list comparison";
        
      assert_equal "true" (peval_no_params "1 <= 1")
        ~msg: "int <= int comparison";
      assert_equal "true" (peval_no_params "0.5 >= 0.0")
        ~msg: "float >= float comparison";
      assert_equal "true" (peval_no_params "\"hell\" <= \"hello\"")
        ~msg: "string <= string comparison";
      assert_equal "true" (peval_no_params "'b' >= 'b'")
        ~msg: "char >= char comparison";
      assert_equal (EXP(Boolean true)) (eval_expression_no_params "[1;2] <= [1;2]")
        ~msg: "list <= list comparison"
        ~printer: string_of_expression;
    );
    "Exceptions tests" >:: (fun () ->
      assert_raises
        (InvalidParameterType (1, "string or character or list or dict"))
        (fun () -> peval_no_params("len(200)"))
        ~msg: "Bad len: invalid parameter type";
      assert_raises
        (InvalidParameterType (2, "numeric"))
        (fun () -> peval_no_params ("1 + true"))
        ~msg: "Bad addition: invalid parameter type (numeric)";
      assert_raises
        (InvalidParameterType (2, "dict"))
        (fun () -> peval_no_params ("{} + 30.5"))
        ~msg: "Bad addition: invalid parameter type (dict)";
      assert_raises
        (NotDeclaredFunctionError "fire_missiles")
        (fun () -> peval_no_params ("fire_missiles(200)"))
        ~msg: "Not declared function";
    );
    "Complex expressions test" >:: (fun () ->
      assert_equal
        "300.5"
        (peval_no_params ( 
          "(10.0 * [1;2;3;4;5;6].[4] + 100.0) * 2 " ^
          " + {\"half\": 0.5; \"third\": 0.33}.{\"half\"}"
        ))
        ~msg: "Complex expression 1";
      assert_equal
        "false"
        (peval_no_params "not 6 <= len([true;'a' + \"hello\";1 + 3].[10 - 9])")
        ~msg: "Complex expression 2";
    );
    "Variable binding test" >:: (fun() ->
      assert_equal
        "20"
        (peval "2 * num"
          [ ("num", EXP (Numeric (Int 10)));
            ("lala", EXP (Boolean true))
          ]
        )
        ~msg: "Variable binding";
      assert_equal 
        "hello world"
        (peval "h + ' ' + w"
          [ ("h", EXP (String "hello"));
            ("w", EXP (String "world"))
          ] 
        )
        ~msg: "Multiple variable binding";
      assert_raises
        (UnboundVariable "foo")
        (fun () -> peval "1 + foo" [])
        ~msg: "Unbound variable testing"
    );
		"File parsing tests" >:: (fun () ->
			assert_equal
				(file_to_string "test_expression_evaluator_result.html") 
				(parse_file "test_expression_evaluator.html" [])
				~msg: "Complete File Test";
    )
  ];;
  
let test () = run_test_tt ~verbose:true tests;;

