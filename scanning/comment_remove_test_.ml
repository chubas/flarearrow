open OUnit;;
open Comment_remove;;
open Exceptions;;
open Flarelib;;

let text_fixture ="Comment remove suite" >:::
	[
		"Test inicio" >:: (fun()->
			assert_equal "<html>\r\n\t<head>\r\n\t\t<title></title>\r\n\t</head>\r\n\t<body>\r\n\t\ttext\r\n\t\t           \r\n\t\tmore                  text\r\n\t\t{{ {# does nothing #} }}\r\n\t</body>\r\n</html>\r\n"
			(remove "comment_remove_test_one.html");
			);
		"Test error" >:: (fun()->
			assert_raises NotTerminatedString (fun ()-> stateA (string_to_list "Not terminated{#") "");
			);
	]
	
let _= run_test_tt ~verbose:true text_fixture;   