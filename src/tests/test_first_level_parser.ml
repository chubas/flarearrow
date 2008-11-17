open Position;;
open Grammar_flp;;
open First_level_parser;; 
open Template_evaluator;;
open OUnit;;

let flp_fixture = "First level parser suite" >:::
  [
    "Simple tests" >:: (fun () ->
      assert_equal
        [RAWTEXT ("This is raw text", Position (1, 0))]
        (tokens_from_string "This is raw text")
        ~msg:"Raw test test";
      assert_equal
        [COMMENT (" This is a COMMENT ", Position (1, 2))]
        (tokens_from_string "{# This is a COMMENT #}")
        ~msg:"Single COMMENT test";
      assert_equal
        [EXPRESSION (" This is an EXPRESSION ", Position (1, 2))]
        (tokens_from_string "{{ This is an EXPRESSION }}")
        ~msg:"Single EXPRESSION test";
      assert_equal
        [COMMENT (" This is a multiline\nCOMMENT ", Position(1, 2))]
        (tokens_from_string "{# This is a multiline\nCOMMENT #}")
        ~msg:"Multiline COMMENT test";
      assert_equal
        [EXPRESSION (" This is a multiline\nEXPRESSION ", Position(1, 2))]
        (tokens_from_string "{{ This is a multiline\nEXPRESSION }}")
        ~msg:"Multiline EXPRESSION test";
      assert_equal
        [
          RAWTEXT ("This is raw text ", Position (1, 0));
          EXPRESSION (" followed by an EXPRESSION ", Position (1, 19))
        ]
        (tokens_from_string "This is raw text {{ followed by an EXPRESSION }}")
        ~msg:"Raw text combined with EXPRESSION"
    );
    "Unfinished blocks test" >:: (fun () ->
      assert_raises
        (NotTerminatedComment (Position (1, 2)))
        (fun () ->
          (tokens_from_string "{# Not terminated COMMENT")
        )
        ~msg:"Not terminated COMMENT";
      assert_raises
        (NotTerminatedExpression (Position (1, 2)))
        (fun () ->
          (tokens_from_string "{{ Not terminated COMMENT")
        )
        ~msg:"Not terminated EXPRESSION"; 
    );
    "Strings tests" >:: (fun () ->
      assert_equal
        [COMMENT (" \"string in a COMMENT \" ", Position (1, 2))]
        (tokens_from_string "{# \"string in a COMMENT \" #}")
        ~msg:"String in a COMMENT test";
      assert_equal
        [EXPRESSION (" \"string in an EXPRESSION\" ", Position (1, 2))]
        (tokens_from_string "{{ \"string in an EXPRESSION\" }}")
        ~msg:"String in an EXPRESSION";
    );
    "String exception tests" >:: (fun () ->
      assert_raises
        (NotTerminatedString (Position (1, 4)))
        (fun () ->
          (tokens_from_string "{{ \"not terminated string... }}")
        )
        ~msg:"Not terminated string";
      assert_equal
        [RAWTEXT ("\"Unfinished string in raw text mode...", Position (1, 0))]
        (tokens_from_string "\"Unfinished string in raw text mode...")
        ~msg:"Unfinished strings in raw text should not cause an exception"
    );
    "Escaped special characters tests" >:: (fun () ->
      assert_equal
        [COMMENT (" Special \\#} sequence inside COMMENT ", Position (1, 2))]
        (tokens_from_string "{# Special \\#} sequence inside COMMENT #}")
        ~msg:"Escaped end of COMMENT block";
      assert_equal
        [EXPRESSION (" Special \\}} sequence inside EXPRESSION ", Position (1, 2))]
        (tokens_from_string "{{ Special \\}} sequence inside EXPRESSION }}")
        ~msg:"Escaped end of EXPRESSION block";
      assert_equal
        [COMMENT (" Quoted \"#}\" sequence inside COMMENT ", Position (1, 2))]
        (tokens_from_string "{# Quoted \"#}\" sequence inside COMMENT #}")
        ~msg:"Quoted end of COMMENT block";
      assert_equal
        [EXPRESSION (" Quoted \"}}\" sequence inside EXPRESSION ", Position (1, 2))]
        (tokens_from_string "{{ Quoted \"}}\" sequence inside EXPRESSION }}")
        ~msg:"Quoted end of EXPRESSION block"
    );
    "File lexing test" >:: (fun () ->
      assert_raises
        (Sys_error "foo.bar.baz: No such file or directory")
        (fun () ->
          tokens_from_file "foo.bar.baz"
        )
        ~msg:"Non existant files testing";
     assert_equal
        [
          RAWTEXT ("<html>\n\t<head>\n\t\t<title> ", Position (1, 0));
					EXPRESSION (" title ", Position (3, 12));
					RAWTEXT (" </title>\n\t\t", Position (3, 21));
					COMMENT (" Include css files here ", Position (4, 4));
					RAWTEXT ("\n\t</head>\n\t<body>\n\t\t", Position (4, 30));
					EXPRESSION (" for letter in \"String\" ", Position (7, 4));
					RAWTEXT ("\n\t\t\t", Position (7, 30));
					EXPRESSION (" => letter ", Position (8, 5));
					RAWTEXT ("\n\t\t\tMore content\n\t\t\t", Position (8, 18));
					EXPRESSION (" multiline\n\t\t\t\texpression ", Position (10, 5));
					RAWTEXT ("\n\t\t", Position (11, 17));
					EXPRESSION (" end ", Position (12, 4));
					RAWTEXT ("\n\t\tContent content content\n\t\t", Position (12, 11));
					COMMENT (" Don't forget escaped \\#} sequences ", Position (14, 4));
					RAWTEXT ("\n\t\t", Position (14, 42));
					EXPRESSION (" and \"quoted }}\" sequences ", Position (15, 4));
					RAWTEXT ("\n\t</body>\n</html> ", Position (15, 33));
					COMMENT (" End of test file ", Position (17, 10))
        ]
       (tokens_from_file "test_first_level_parser.html")
    )
  ];;
  
let test () = run_test_tt ~verbose:true flp_fixture;;
