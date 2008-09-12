open OUnit;;
open Comment_remove;;
open Exceptions;;
open Flarelib;;

let text_fixture ="Comment remove suite" >:::
	[
		"Test inicio" >:: (fun()->
			assert_equal "<html>\n\t<head>\n\t\t<title></title>\n\t</head>\n\t<body>\n\t\ttext\n\t\t           \n\t\tmore                  text\n\t\t{{ {# does nothing #} }}\n\t</body>\n</html>\n"
			(remove "comment_remove_test_one.html");
			);
		"Test error" >:: (fun()->
			assert_raises NotTerminatedString (fun ()-> stateA (string_to_list "Not terminated{#") "");
			);
	]
	
let _= run_test_tt ~verbose:true text_fixture;  