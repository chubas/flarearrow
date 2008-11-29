open Mysql;;
open Flarelib;;

let salt    = "w00t";;

let pwdstring s = md5 (s ^ salt);;  

exception UserDoesNotExist of string;;
exception DuplicatedUser of string;;
exception SessionNotFound;;

let check_user_exists db_connection username =
  let result = 
    Mysql.exec db_connection 
    ("select count(*) from users where username like binary " ^ 
      (Mysql.ml2str username))
  in
    match Mysql.fetch result with
      | None -> assert false
      | Some results -> (match Array.to_list results with
        | [Some "0"] -> false
        | [Some "1"] -> true
        | [Some _  ] -> raise (DuplicatedUser username)
        (* We got more than one user with that name *)
        | _          -> assert false
        (* We got more than a result, which should not occur *)
      ) 
;;

let user_authenticates db_connection username password =
  let result = Mysql.exec db_connection
    ("select * from users where username like binary " ^ (Mysql.ml2str username) ^ 
     " and password like binary " ^ (Mysql.ml2str (pwdstring password) ^ ";"))
  in
    match Mysql.size result with
      | 0L -> false
      | 1L -> true
      | _  -> raise (DuplicatedUser username)
;;

let user_stats db_connection username =
  let (current_tries, current_corrects) = 
    let result = Mysql.exec db_connection
      ("select tries, corrects from users where username like binary " ^ 
        (Mysql.ml2str username)) in
    match fetch result with
      | None -> raise (UserDoesNotExist username)
      | Some results -> (match Array.to_list results with
        | [Some _tries; Some _corrects] -> 
            (int_of_string _tries, int_of_string _corrects)
        | _ -> assert false
      ) in
  (current_tries, current_corrects)
;;
  
let user_answered db_connection username is_correct =
  let (current_tries, current_corrects) = user_stats db_connection username in
  let new_tries = current_tries + 1 in
  let new_corrects = current_corrects + (if is_correct then 1 else 0) in
  ignore (
    Mysql.exec db_connection
    ("update users set tries = " ^ (Mysql.ml2int new_tries) ^
     ", corrects = " ^ (Mysql.ml2int new_corrects) ^ 
     " where username = " ^ (Mysql.ml2str username)));
    (new_tries, new_corrects)
;;
  
let print_query db_connection query = 
	let result = Mysql.exec db_connection query in
		Mysql.iter result
		  (fun arr -> 
		    let lst = Array.to_list arr in
		    let m = List.map (function None -> "~" | Some s -> s) lst in
        let pr _ = print_endline "---" in
        pr (); List.iter (print_endline) m; pr ()
	  );;

let insert_user db_connection username password =
  if check_user_exists db_connection username then
    raise (DuplicatedUser username)
  else
	  let query = "insert into users values " ^
	    (Mysql.values
	      [Mysql.ml2str username;
	       Mysql.ml2str (pwdstring password);
	       Mysql.ml2int 0;
	       Mysql.ml2int 0
	      ]) ^
	    ";" in
	  ignore (Mysql.exec db_connection query)
;;

let store_session db_connection session_id username =
  let query = "insert into sessions values " ^
    (Mysql.values [Mysql.ml2str session_id; Mysql.ml2str username]) ^
    ";" in
  ignore (Mysql.exec db_connection query)
;;

let retrieve_user db_connection session_id =
  let result = Mysql.exec db_connection
    ("select authenticated_user from sessions where session_id like binary " ^
      (Mysql.ml2str session_id) ^ ";")
  in
    match Mysql.fetch result with
      | None -> raise Not_found
      | Some results ->
        (match Array.get results 0 with
          | Some username -> username
          | _ -> assert false
        )
;;

let flickr_api_key = "3b6c7c141f55fad34ae21ac70ad3dbbc";;
let flickr_secret  = "7689c959aa5eada2";;

let word_list = 
  (* List.map (Netencoding.Url.encode) *) 
  (* Encoding should be handler by the javascript? *)
  [
    "apple";"baby";"back";"ball";"bear";"bed";"bell";"bird";
    "birthday";"boat";"box";"boy";"bread";"brother";"cake";
    "car";"cat";"chair";"chicken";"children";"christmas";
    "coat";"corn";"cow";"day";"dog";"doll";"door";"duck";"egg";
    "eye";"farm";"farmer";"father";"feet";"fire";"fish";
    "floor";"flower";"game";"garden";"girl";"goodbye";
    "grass";"ground";"hand";"head";"hill";"home";"horse";
    "house";"kitty";"leg";"letter";"man";"men";"milk";"money";
    "morning";"mother";"name";"nest";"night";"paper";"party";
    "picture";"pig";"rabbit";"rain";"ring";"santa claus";
    "school";"seed";"sheep";"shoe";"sister";"snow";"song";
    "squirrel";"stick";"street";"sun";"table";"thing";"time";
    "top";"toy";"tree";"watch";"water";"way";"wind";"window";
    "wood"; "zebra"; "chocolate"; "candy"; "gamer";
    "color"; "computer"; "videogame"; "stomach";
    "carrot"; "pancake"; "snake"; "desktop";
    "spoon"; "world"; "pacman"; "magazine";
    "big ben"; "rolling stones"; "spongebob";
    "lion"; "hot dog"; "soccer"; "south park";
    "mario bros"; "scotland"; "frog"; "windows";
    "blue"; "red"; "yellow"; "green"; "black"; "pink";
    "television"; "children"; "radio"; "triangle";
    "kinder"; "ferrari"; "paperclip"; "bush";
    "scissors"; "pokemon"; "blackberry"; "youtube";
    "blog"; "lifehacker"; "train"; "star wars";
    "fox"; "monkey"; "ice cream"; "zoo"; "explosion";
    "doll"; "skyscrapper"; "rock"; "lunch"; "rocket";
    "shirt"; "batman"; "spiderman"; "x-men"; "watchmen";
    "japan"; "mexico"; "india"; "china"; "australia";
    "netherlands"; "germany"; "russia"; "spain"; "wtf";
    "coffee"; "milk"; "jesus"; "alcohol"; "glass"; "plate";
    "letters"; "firefighter"; "astronaut"; "vodka";
    "screen"; "scream"; "cry"; "laugh"; "fall"; "smile";
    "run"; "driver"; "motorcycle"; "bus"; "bell"; "tire";
    "motor"; "insect"; "box"; "domo"; "anime"; "backpack";
    "schoolbag"; "ruler"; "pencil"; "pen"; "elephant";
    "kangaroo"; "giraffe"; "lettuce"; "drink"; "fireworks";
    "laptop"; "pear"; "cigar"; "orange"; "juice";
    "pinneapple"; "pizza"; "breakfast"; "clock";
    "metal"; "wood"; "sleep"; "big"; "star"; "sun";
    "moon"; "jupiter"; "mars"; "ball"; "sunset"; "rainbow";
    "gold"; "honey"; "bee"; "ant"; "hive"; "knife";
    "fork"; "saw"; "jigsaw"; "controller"; "transformers";
    "brush"; "paint"; "minesweeper"; "blackjack";
    "chess"; "knight"; "queen"; "princess"; "king";
    "toadstool"; "peach"; "random"; "think"; "tag";
    "brain"; "goal"; "tennis"; "basketball"; "swim";
    "NullPointerException"; "swan"; "duck"; "eagle";
    "platipus"; "key"; "keychain"; "yoshi"; "ring";
    "people"; "cell phone"; "telephone"; "crayon";
    "basket"; "sushi"; "rice"; "taco"; "burrito";
    "sweater"; "pants"; "socks"; "trousers"; "underwear";
    "nail"; "hammer"; "shovel"; "farm"; "train";
    "storm"; "cloud"; "class"; "shower"; "music";
    "piano"; "guitar"; "violin"; "fire"; "water";
    "bass"; "kitchen"; "cookie"; "table"; "sesame street";
    "monster"; "devil"; "witch"; "ghost"; "vampire";
    "werewolf"; "wolf"; "mozart"; "beethoven"; "heart";
    "dance"; "einstein"; "newton"; "physics"; "atom";
    "flickr"; "google"; "yahoo"; "microsoft";
    "facebook"; "spam"; "worm"; "mail"; "movie";
    "mask"; "chair"; "iron"; "obama"; "iron man";
    "hair"; "ear"; "finger"; "foot"; "root";
    "fountain"; "lake"; "gnome"; "waterfall"; "rain";
    "forest"; "gorilla"; "jungle"; "gun"; "rose";
    "church"; "turing"; "thesis"; "work"; "warcraft";
    "artist"; "france"; "cheese"; "mouse"; "moose";
    "horn"; "unicorn"; "siren"; "sea"; "shell";
    "squid"; "fish"; "bubble"; "starfish"; "shark";
    "whale"; "dragon ball"; "fight"; "blood";
    "street"; "thumb"; "boat"; "road"; "flag";
    "golf"
  ]
;;
