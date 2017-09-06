(*

size:       15 * n + 14        3 * n^2 + 20 * n + 15 (od2)
index:      8 * n + 20

iterations: 16 * 2^n - 10 (si)  13 * 2^n - 10 (os)
od2         15 * 2^n - 6       14 * 2^n - 11

*)

let show_help _ =
	print_string (Info.get_title "Super-Polynomial Strategy Iteration Generator");
	print_string ("Usage: superpolystratimpr n [od2]\n\n" ^
	              "       where n = n-th super-polynomial strategy iteration game\n" ^
	              "             od2 = optional parameter; enabled out-degree 2 limitation\n\n" ^
	              "Notes: - If you want to use it directly with PGSolver, you need to\n" ^
	              "           - either disable scc decomposition, global and local optimization,\n" ^
	              "             i.e. -dsd -dgo -dlo\n" ^
	              "           - or transform the game using the transformer tool as follows:\n" ^
	              "             transformer -ss -ap -bn\n" ^
	              "       - If you want to use it with the journal version of Schewe's\n" ^
	              "         algorithm, you need to transform the game as follows:\n" ^
	              "         transformer -pa -dn -ip -al\n" ^
	              "\n")

open Paritygame;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*b*)
			  | DecLaneEvenRoot (*c*)
			  | DecLaneOddRoot (*d*)
			  | CycleNode0 of int (*e*)
			  | CycleNode1 of int (*f*)
			  | CycleNode2 of int (*g*)
			  | CycleCenter of int (*h*)
			  | CycleAccess of int (*k*)
			  | CycleSelector of int (*l*)
			  | CycleLeaver of int (*m*)
			  | UpperSelector of int (*z*)
			  | FinalSink (*p*)
			  | FinalCycle (*q*)
			  | StartEven (*s*)
			  | StartSelector (*t*)
			  | HighEntryBit (*w*)
			  | LowestBit (*y*)
			  | BitSelector (*x*)
			  | Helper of int

let symb_to_str = function DecLaneEven i ->"a" ^ string_of_int i | DecLaneOdd i ->"b" ^ string_of_int i | DecLaneEvenRoot ->"c" | DecLaneOddRoot ->"d" | CycleNode0 i ->"e" ^ string_of_int i | CycleNode1 i ->"f" ^ string_of_int i | CycleNode2 i ->"g" ^ string_of_int i | CycleCenter i ->"h" ^ string_of_int i | CycleAccess i ->"k" ^ string_of_int i | CycleSelector i ->"l" ^ string_of_int i | CycleLeaver i ->"m" ^ string_of_int i | UpperSelector i ->"z" ^ string_of_int i | FinalSink ->"p" | FinalCycle ->"q" | StartEven ->"s" | StartSelector ->"t" | HighEntryBit ->"w" | LowestBit ->"y" | BitSelector ->"x" | Helper i ->"h" ^ string_of_int i

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments = 

	let helpercnt = ref 0 in

	let od2 = ref false in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

	let rec right_exp = function [] -> failwith "imp" | [a] -> a
	|	a::r -> incr helpercnt; add (Helper !helpercnt) 0 0 [a; right_exp r]; Helper !helpercnt
	in
	
	let rec left_exp = function [] -> failwith "imp" | [a] -> a
	|	a::b::r -> incr helpercnt; add (Helper !helpercnt) 0 0 [a; b]; left_exp ((Helper !helpercnt)::r)
	in
	
	let left_exp' l =
		if List.length l <= 2 then l
		else let r = List.rev l in
			 let (h, t) = (List.hd r, List.rev (List.tl r)) in
			 [left_exp t; h]
	in
	(*
	let right_exp' l =
		if List.length l <= 2 then l
		else let (h, t) = (List.hd l, List.tl l) in
			 [h; right_exp t]

	let rexp l = if !od2 then right_exp' l else l
	*)
	let lexp l = if !od2 then left_exp' l else l in

    if (Array.length arguments < 1) || (Array.length arguments > 2) then (show_help (); exit 1);

	let n = int_of_string arguments.(0) in
	od2 := Array.length arguments > 1;

	add FinalCycle 1 1 [FinalCycle];
	add StartEven 2 0 (lexp (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add StartSelector 3 0 [BitSelector; StartEven];
	add LowestBit 5 0 [HighEntryBit; CycleSelector 0];
	add HighEntryBit 7 0 (lexp (LowestBit::FinalSink::(mkli (n - 1) (fun j -> CycleSelector (j + 1)))));
	add DecLaneEvenRoot (12 * n + 11) 1 [DecLaneOddRoot];
	add DecLaneOddRoot (12 * n + 12) 0 [StartSelector; BitSelector];
	add BitSelector (12 * n + 14) 0 [HighEntryBit; LowestBit];
	add FinalSink (16 * n + 16) 1 [FinalCycle];

	for i = 0 to 3 * n do
		add (DecLaneEven i) (6 * n + 10 + 2 * i) 1 [DecLaneOdd i];
		add (DecLaneOdd i) (6 * n + 9 + 2 * i) 0 (lexp [(if i = 0 then DecLaneOddRoot else DecLaneOdd (i - 1)); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		add (CycleNode0 i) (i * 6 + 9) 0 (lexp (([StartEven; CycleNode1 i; DecLaneEvenRoot] @ (mkli (i + 1) (fun j -> DecLaneEven (3 * j + 2))))));
		add (CycleNode1 i) (i * 6 + 11) 0 (lexp (((CycleNode2 i)::(mkli (i + 1) (fun j -> DecLaneEven (3 * j + 1))))@(mkli n (fun j -> CycleAccess j))));
		add (CycleNode2 i) (i * 6 + 13) 0 (lexp (((CycleCenter i)::(mkli (i + 2) (fun j -> DecLaneEven (3 * j))))));
		add (CycleCenter i) (i * 6 + 14) 1 [CycleNode0 i; CycleLeaver i];
		add (CycleLeaver i) (12 * n + 18 + 4 * i) 1 [UpperSelector i];
		add (UpperSelector i) (12 * n + 15 + 4 * i) 0 (lexp (FinalSink::(mkli (n - i - 1) (fun j -> CycleSelector (n - j - 1)))));
        add (CycleAccess i) (12 * n + 17 + 4 * i) 1 [CycleCenter i];
        add (CycleSelector i) (i * 6 + 10) 0 [CycleAccess i; UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;

Generators.register_generator generator_game_func "superpolystratimpr" "Super-polynomial Strategy Iteration Game";;