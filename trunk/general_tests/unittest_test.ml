(* Run it with the following options *)
(* -I /path/to/ounit/ [unix.cma] oUnit.cma unittest_test.ml *)

open OUnit

let test_suite = "Example test suite" >:::
  [
    "Test 1" >:: (fun () ->
      assert_equal 1 (3 - 2);
      assert_equal 10 (2 * 5);
      assert_equal "foobar" ("foo" ^ "bar")
    );
    "Test 2" >:: (fun () ->
      assert_equal "No" "No";
      assert_equal "Pass test" "Pass test";
      assert_equal "This test is going to fail" "Yes"
    )
  ]

let _ = run_test_tt ~verbose:true test_suite;;
