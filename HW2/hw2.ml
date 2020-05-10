type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
;;

type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
;;

let rec make_rules nt r =
	match r with
	| [] -> []
	| (n, ru)::rules_left ->
		if n == nt
			then ru::(make_rules nt rules_left)
		else make_rules nt rules_left
;;

let convert_grammar gram1 = (fst gram1), (function nt -> make_rules nt (snd gram1));;

let parse_tree_leaves tree =
	let rec ex = function
		| [] -> []
		| head::tail -> (h head) @ (ex tail)
	and h = function
		| Leaf l -> [l]
		| Node (n, it) -> (ex it)
		in ex [tree]
;;

let rec helper f r a g = 
	match r with
  		| [] -> None 
  		| head::rules_left ->  
      	let o = (m f head) a g in	
		match o  with 
      		| None -> (helper f rules_left) a g
      		| _ -> o
    	and m f r a g =
      	match r with 
      	| (T y)::left -> 
          	if (List.length g = 0) then None else 
            	(if (List.hd(g) = y) 
                	then (m f left a (List.tl(g))) else None)
      	| (N z)::left -> 
          	helper f (f z) (m f left a) g
      	| [] -> a g
;;

let make_matcher gram =
	match gram with
	| (n, f) ->
		(fun a fr -> helper f (f n) a fr)
;;

let rec make_parser gram frag =
	let take frag a = 
		match frag with
		| _::_ -> None
		| x -> Some (x, a) in
	let rec d s r apt frag a = function
		| [] -> None
		| fr::rem_rules ->
			match (dd r fr apt frag ((s, fr)::a)) with
				| None -> d s r apt frag a rem_rules
				| sth -> sth
	and dd r u apt frag a = 
		match u with
		| [] -> apt frag a
		| _ -> match frag with
			| [] -> None
			| ff::rem_f -> match u with 
				| [] -> None
				| (N nt)::rem_s -> d nt r
					(dd r rem_s apt) frag a (r nt)
				| (T t)::rem_sm ->
					if ff = t 
						then dd r rem_sm apt rem_f a
					else None in
	let rec p r = 
		match r with
		| [] -> Node (fst (List.hd r), fst (pp r [])), snd (pp r [])
		| head::tail -> Node (fst head, fst (pp tail (snd head))), snd (pp tail (snd head))
	and pp r = function
		| [] -> [], r
		| head::tail -> match head with
			| N _ -> (fst (p r))::(fst (pp (snd (p r)) tail)), snd (pp (snd (p r)) tail)
			| T terminal ->
	 			(Leaf terminal)::(fst (pp r tail)), snd (pp r tail) in
	match (d (fst gram) (snd gram) take frag [] ((snd gram) (fst gram))) with
		| Some (_, r) -> Some (fst (p (List.rev r)))
		| _ -> None 
;;
