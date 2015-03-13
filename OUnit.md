# OUnit #

You can download OUnit from: http://www.xs4all.nl/~mmzeeman/ocaml/

A very nice and simple video tutorial available at: http://www.xs4all.nl/~mmzeeman/ocaml/


An example:

Given two modules as follows:
(NOTE: The modules don't need any special names)

```
(* SimpleMath.ml *)
let add a b = a + b
let sub a b = a - b

(*End of SimpleMath.ml *)



(* Test_SimpleMath.ml *)

open OUnit;;

let text_fixture = "SimpleTest" >:::
[
  "add" >:: (fun () -> 
    assert_equal 4 (SimpleTest.add 2 2); (* PASS *)
    assert_equal 0 (SimpleTest.add 0 0); (* PASS *)
    ); 
  "substract" >:: (fun () -> 
    assert_equal 4 (SimpleTest.sub 4 4); (* FAIL *)
    assert_equal 1 (SimpleTest.sub 5 4); (* PASS *)
    );
]


(* Test Runner *)
let _ = run_test_tt ~verbose:true text_fixture;

(* End of Test_SimpleMath.ml *)

```


Just compile and run Test\_SimpleMath.ml

(You may need to load oUnit.cma and unix.cma )
If using Eclipse with OcaIDE follow the steps in [this video](http://www.youtube.com/watch?v=5Om2aEijfls)