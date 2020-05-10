type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec subset a b =
	match a with
	| [] -> true
	| head::tail -> (List.mem head b) && subset tail b;;

let equal_sets a b =
	subset a b && subset b a;;

let set_union a b =
	List.sort_uniq compare (List.append a b);;

let rec set_intersection a b =
	match a with
	| [] -> []
	| head::tail -> if List.mem head b then head::(set_intersection tail b)
			else (set_intersection tail b);;

let rec set_diff a b =
	match a with
	| [] -> []
	| head::tail -> if (List.mem head b) then set_diff tail b
			else head::(set_diff tail b);;

let rec computed_fixed_point eq f x =
	if eq (f x) x then x
	else computed_fixed_point (eq) (f) (f x);;



let rec filter_nonterminals rules = 
	match rules with
	| [] -> []
	| N head::rem_rules -> head::(filter_nonterminals rem_rules)
	| _::rem_rules -> filter_nonterminals rem_rules;;

let rec create_nonterminals nt rules =
	match rules with
	| [] -> nt
	| r_t::tuples_left ->
		if (List.mem (fst r_t) nt) then create_nonterminals (set_union nt (filter_nonterminals (snd r_t))) tuples_left
		else create_nonterminals nt tuples_left;;

let rec find_nts nts rules =
        let o = create_nonterminals nts rules in
                let t = create_nonterminals o rules in
                        if equal_sets o t then t
                        else find_nts t rules;;

let filter_rules s rules =
	let rnt = (find_nts [s] rules) in
		List.filter (fun x -> List.mem (fst x) rnt) rules;;

let filter_reachable g = ( (fst g), (filter_rules (fst g) (snd g) ));;  
