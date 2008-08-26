(* 
 * Function that accepts a list of tuples (N, E) or single elements and 
 * expands them according to N
 * decode [("A", 2); ("B", 5); ("C", 4); "D"; ("E", 3); "F"]
 *     =>  ["A"; "A"; "B"; "B"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "D"; "E"; "E"; "E"; "F"]
 *)


(* Works if we only send tuple parameters, i.e. (lst : ('a * 'b) *)
let rec decode_r lst acum = match lst with
    [] -> acum
  | (_, 0)::t -> decode_r t acum
  | (e, n)::t -> decode_r ((e, n-1)::t) (acum @ [e]);;

let decode lst = decode_r lst [];;
  
List.iter (fun x -> Printf.printf "%s " x) (decode [("A", 4); ("B", 5); ("C", 1); ("X", 10)]);;
Printf.printf "\n";;

(* How to make it work with tuples and single elements? *)
(* Using types! *)

type 'a t = Element of 'a | Tuple of ('a * int);;

let rec decode2_r lst acum = match lst with
    [] -> acum
  | (Tuple (_, 0))::t  -> decode2_r t acum
  | (Tuple (e, n))::t  -> decode2_r ((Tuple (e, n-1))::t) (acum @ [e])
  | (Element e)::t -> decode2_r t (acum @ [e]);;
  
let decode2 lst = decode2_r lst [];;
  
List.iter (fun x -> Printf.printf "%s " x) (decode2
  [Tuple ("A", 4);
   Tuple ("B", 5);
   Element "C";
   Tuple ("D", 10);
   Element "X";
   Tuple ("Y", 10);
   Element "Z"]);;
Printf.printf "\n";;
