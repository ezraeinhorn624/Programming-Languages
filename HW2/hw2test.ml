let accept_all string = Some string;;

type test_nonterms =
  | Red | Orange | Yellow | Green | Blue

let test_grammar = 
  (Red,
  function
    | Red -> 
	[[N Green]; 
	[N Blue; N Orange]; 
	[N Yellow; N Green; N Blue]]
    | Orange -> 
	[[N Green]; 
	[N Yellow; T "purple"]]
    | Yellow -> 	
	[[T "purple"; T "pink"]; 
	[N Blue]; 
	[N Green]]
    | Green -> 
	[[T "white"];
	[N Blue]]
    | Blue -> 
	[[T "indigo"]])
;;

let t = ["purple"; "pink"; "white"; "indigo"]
;;

let make_matcher_test = 
	make_matcher test_grammar accept_all t = Some []
;;

let make_parser_test = 
	match make_parser test_grammar t with 
	| Some tree -> parse_tree_leaves tree = t
	| _ -> false
;;
