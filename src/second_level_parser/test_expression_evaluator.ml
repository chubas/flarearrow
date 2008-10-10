open Expression_evaluator;;
open Flarelib;;
open OUnit;;


let test_file_result =file_to_stringf "test_expression_evaluator_result.html";;


let tests = "Expression Evaluator" >:::
  [
    "first test" >:: (fun () ->
      assert_equal
        ("10")
        (peval "5+5")
        ~msg: "Numeric Parse - int";
      assert_equal
        ("50.5")
        (peval "101.0/2.0")
        ~msg: "Numeric Parse - float";
      assert_equal
        ("holamundo")
        (peval "\"hola\"^\"mundo\"")
        ~msg: "String Concat Parse";
      assert_equal
        ("cmundo")
        (peval "'c'^\"mundo\"")
        ~msg: "String-Char Concat Parse";
      assert_equal
        ("true")
        (peval "5+5<=10")
        ~msg: "Comparative Parse";
			assert_equal
				("true")
				(peval "5+5<=10 and 5+5<=29")
				~msg: "Logical Parse";
			assert_equal
				("false")
				(peval " \"hola\" < \"a\" ")
				~msg: "String Compar Parse";
				
    );
		"File Evaluator Tests" >:: (fun () ->
			assert_equal
				(test_file_result)
				(parse_string (file_to_stringf "test_expression_evaluator.html"))
				~msg: "Complete File Test";
			)
  ];;
  
let _= run_test_tt ~verbose:true tests;;
