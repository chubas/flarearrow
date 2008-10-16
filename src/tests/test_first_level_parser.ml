open First_level_parser;; 
open OUnit;;

let flp_fixture = "First level parser suite" >:::
  [
    "Simple tests" >:: (fun () ->
      assert_equal
        [RawText ("This is raw text", Position (1, 0))]
        (tokens_from_string "This is raw text")
        ~msg:"Raw test test";
      assert_equal
        [Comment (" This is a comment ", Position (1, 2))]
        (tokens_from_string "{# This is a comment #}")
        ~msg:"Single comment test";
      assert_equal
        [Expression (" This is an expression ", Position (1, 2))]
        (tokens_from_string "{{ This is an expression }}")
        ~msg:"Single expression test";
      assert_equal
        [Comment (" This is a multiline\ncomment ", Position(1, 2))]
        (tokens_from_string "{# This is a multiline\ncomment #}")
        ~msg:"Multiline comment test";
      assert_equal
        [Expression (" This is a multiline\nexpression ", Position(1, 2))]
        (tokens_from_string "{{ This is a multiline\nexpression }}")
        ~msg:"Multiline expression test";
      assert_equal
        [
          RawText ("This is raw text ", Position (1, 0));
          Expression (" followed by an expression ", Position (1, 19))
        ]
        (tokens_from_string "This is raw text {{ followed by an expression }}")
        ~msg:"Raw text combined with expression"
    );
    "Unfinished blocks test" >:: (fun () ->
      assert_raises
        (NotTerminatedComment (Position (1, 2)))
        (fun () ->
          (tokens_from_string "{# Not terminated comment")
        )
        ~msg:"Not terminated comment";
      assert_raises
        (NotTerminatedExpression (Position (1, 2)))
        (fun () ->
          (tokens_from_string "{{ Not terminated comment")
        )
        ~msg:"Not terminated expression"; 
    );
    "Strings tests" >:: (fun () ->
      assert_equal
        [Comment (" \"string in a comment \" ", Position (1, 2))]
        (tokens_from_string "{# \"string in a comment \" #}")
        ~msg:"String in a comment test";
      assert_equal
        [Expression (" \"string in an expression\" ", Position (1, 2))]
        (tokens_from_string "{{ \"string in an expression\" }}")
        ~msg:"String in an expression";
    );
    "String exception tests" >:: (fun () ->
      assert_raises
        (NotTerminatedString (Position (1, 4)))
        (fun () ->
          (tokens_from_string "{{ \"not terminated string... }}")
        )
        ~msg:"Not terminated string";
      assert_equal
        [RawText ("\"Unfinished string in raw text mode...", Position (1, 0))]
        (tokens_from_string "\"Unfinished string in raw text mode...")
        ~msg:"Unfinished strings in raw text should not cause an exception"
    );
    "Escaped special characters tests" >:: (fun () ->
      assert_equal
        [Comment (" Special \\#} sequence inside comment ", Position (1, 2))]
        (tokens_from_string "{# Special \\#} sequence inside comment #}")
        ~msg:"Escaped end of comment block";
      assert_equal
        [Expression (" Special \\}} sequence inside expression ", Position (1, 2))]
        (tokens_from_string "{{ Special \\}} sequence inside expression }}")
        ~msg:"Escaped end of expression block";
      assert_equal
        [Comment (" Quoted \"#}\" sequence inside comment ", Position (1, 2))]
        (tokens_from_string "{# Quoted \"#}\" sequence inside comment #}")
        ~msg:"Quoted end of comment block";
      assert_equal
        [Expression (" Quoted \"}}\" sequence inside expression ", Position (1, 2))]
        (tokens_from_string "{{ Quoted \"}}\" sequence inside expression }}")
        ~msg:"Quoted end of expression block"
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
          RawText ("<html>\n\t<head>\n\t\t<title> ", Position (1, 0));
					Expression (" title ", Position (3, 12));
					RawText (" </title>\n\t\t", Position (3, 21));
					Comment (" Include css files here ", Position (4, 4));
					RawText ("\n\t</head>\n\t<body>\n\t\t", Position (4, 30));
					Expression (" for letter in \"String\" ", Position (7, 4));
					RawText ("\n\t\t\t", Position (7, 30));
					Expression (" => letter ", Position (8, 5));
					RawText ("\n\t\t\tMore content\n\t\t\t", Position (8, 18));
					Expression (" multiline\n\t\t\t\texpression ", Position (10, 5));
					RawText ("\n\t\t", Position (11, 17));
					Expression (" end ", Position (12, 4));
					RawText ("\n\t\tContent content content\n\t\t", Position (12, 11));
					Comment (" Don't forget escaped \\#} sequences ", Position (14, 4));
					RawText ("\n\t\t", Position (14, 42));
					Expression (" and \"quoted }}\" sequences ", Position (15, 4));
					RawText ("\n\t</body>\n</html> ", Position (15, 33));
					Comment (" End of test file ", Position (17, 10))
        ]
       (tokens_from_file "test_first_level_parser.html")
    )
  ];;
  
let test () = run_test_tt ~verbose:true flp_fixture;;