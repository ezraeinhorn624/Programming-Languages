let subset_test0 = subset [] []
let subset_test1 = subset [] [3;5;8]
let subset_test2 = subset [5;2;2;5] [2;5;7]
let subset_test3 = not (subset [4;8;2] [2;4;6])

let equal_sets_test0 = equal_sets [2;5;6] [5;6;2]
let equal_sets_test1 = equal_sets [2;5;6] [2;5;5;6]
let equal_sets_test2 = not (equal_sets [3;6;17] [4;6;1])

let set_union_test0 = equal_sets (set_union [] [4;5;6]) [4;5;6]
let set_union_test1 = equal_sets (set_union [1;2] [2;5;6]) [1;2;5;6]
let set_union_test2 = equal_sets (set_union [5;5;5;5;5] [5;5]) [5]

let set_intersection_test0 = equal_sets (set_intersection [] [4;2;8]) []
let set_intersection_test1 = equal_sets (set_intersection [5;6;7;8;] [4;8;8;5]) [5;8]
let set_intersection_test2 = not (equal_sets (set_intersection [4;5] [2]) [2;4;5])

let set_diff_test0 = equal_sets (set_diff [5;6] [5;4;6;1]) []
let set_diff_test1 = equal_sets (set_diff [1;5;7] [5;6;7]) [1]

let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 2) 399 = 0

type awksub_nonterminals =
	| A | B | C | D

let awksub_rules = 
	[A, [N B; T "dfh"];
	A, [T "dfh"];
	A, [N C; T "dfh"; N D];
	B, [N C; N B; T "lkd"];
	C, [T "+-"];
	D, [T "sld"]]

let awksub_grammar = A, awksub_rules

let awksub_test0 = filter_reachable awksub_grammar = awksub_grammar
