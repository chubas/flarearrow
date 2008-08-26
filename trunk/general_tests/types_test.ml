(* Types behave like enums *)
type element = Fire | Water | Plant | Ice ;;
type status = Alive | Dead;;

(* Types can be defined as structures *)
type attack = {
  atk_name : string;
  atk_element : element};;
type pokemon = {
  pkm_name : string;
  pkm_element : element};;

(* Polymorphic types are defined as a serie of constructors *)
type move = Move of attack | Defense;;    (* Not used for now... *)

(* Function to determine weaknesses *)
let weakness_for element = match element with
    Fire -> Water
  | Water -> Plant
  | Plant -> Ice
  | Ice -> Fire
  
(* Tuples are structures of different types *)
type pkm = Pokemon of (pokemon * status);;
(* Defining an operator function. Don't you love OCaml? *)
let ( ---> ) attack receiver =
  Printf.printf "%s was attacked by %s!\n" receiver.pkm_name attack.atk_name;
  let is_super_effective = ((weakness_for receiver.pkm_element) = attack.atk_element) in
    Printf.printf "%s\n" (
      if is_super_effective
      then "It's super effective! Your pokemon has died!"
      else "Your pokemon was sightly damaged");
    (receiver, (if is_super_effective then Dead else Alive))
    
(* Pokemon! *)
let squirtle = {pkm_name = "Squirtle"; pkm_element = Water};;
let bulbasaur = {pkm_name = "Bulbasaur"; pkm_element = Plant};;
let articuno = {pkm_name = "Articuno"; pkm_element = Ice};;
let charizard = {pkm_name = "Charizard"; pkm_element = Fire};;

(* Attacks *)
let water_gun = {atk_name = "Water gun"; atk_element = Water};;
let flamethrower = {atk_name = "Flamethrower"; atk_element = Fire};;
let blizzard = {atk_name = "Blizzard"; atk_element = Ice};;
let razor_leaf = {atk_name = "Razor leaf"; atk_element = Plant};;
    
(* Let the fun begin! *)
razor_leaf ---> squirtle;;
water_gun ---> articuno;;
let poor_charizard = water_gun ---> charizard in
  let is_dead = (snd poor_charizard = Dead) in
    Printf.printf "%s\n" (if is_dead then "Poor Charizard =(" else "Yay Charizard! =)");;
