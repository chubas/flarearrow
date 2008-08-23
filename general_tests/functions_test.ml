(* Anonymous functions *)
Printf.printf "%i\n" ((fun x y -> x - y) 10 6);;         (* - : int = 4 *)

(* Operators as functions *)
let do_it operand x y = (if operand = "Sum" then (+) else (-)) x y;;
Printf.printf "%i\n" (do_it "Sum" 200 50);;              (* - : int = 250 *)
Printf.printf "%i\n" (do_it "No sum..." 200 50);;        (* - : int = 150 *)
