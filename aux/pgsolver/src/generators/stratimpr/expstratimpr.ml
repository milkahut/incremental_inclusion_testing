(*

nodes:             25 * n + 10
edges:		       60 * n + 10
index:             26 * n + 16
iterations voege:  20 * 2^n - 16
iterations schewe: 19 * 2^n - 15

*)

let show_help _ =
	print_string (Info.get_title "Exponential Strategy Iteration Generator");
	print_string ("Usage: expstratimpr n\n\n" ^
	              "       where n = n-th exponential strategy iteration game\n" ^
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
			  | DecLaneOddRoot (*d*)
			  | CycleNode0 of int (*e*)
			  | CycleNode1 of int (*f*)
			  | CycleNode2 of int (*g*)
			  | CycleNodeToLane0 of int (*e'*)
			  | CycleNodeToLane1 of int (*f'*)
			  | CycleNodeToLane2 of int (*g'*)  
			  | CycleCenter of int (*h*)
			  | CycleAccessDistributer (* c *)
			  | CycleAccess of int (*k*)
			  | CycleSelector of int (*l*)
			  | CycleSelectorDecelerator of int (*r*)
			  | CycleLeaver of int (*m*)
			  | UpperSelector of int (*z*)
			  | UpperSelectorHelper of int (*v*)
			  | FinalSink (*p*)
			  | FinalCycle (*q*)
			  | StartEven (*s*)
			  | StartSelector (*t*)
			  | HighEntryBit (*w*)
			  | LowestBit (*y*)
			  | BitSelector (*x*)

let symb_to_str = function DecLaneEven i ->"a" ^ string_of_int i | DecLaneOdd i ->"b" ^ string_of_int i | DecLaneOddRoot ->"d" | CycleNode0 i ->"e" ^ string_of_int i | CycleNode1 i ->"f" ^ string_of_int i | CycleNode2 i ->"g" ^ string_of_int i | CycleNodeToLane0 i ->"e'" ^ string_of_int i | CycleNodeToLane1 i ->"f'" ^ string_of_int i | CycleNodeToLane2 i ->"g'" ^ string_of_int i | CycleCenter i ->"h" ^ string_of_int i | CycleAccessDistributer ->"c" | CycleAccess i ->"k" ^ string_of_int i | CycleSelector i ->"l" ^ string_of_int i | CycleSelectorDecelerator i ->"r" ^ string_of_int i | CycleLeaver i ->"m" ^ string_of_int i | UpperSelector i ->"z" ^ string_of_int i | UpperSelectorHelper i -> "v" ^ string_of_int i | FinalSink ->"p" | FinalCycle ->"q" | StartEven ->"s" | StartSelector ->"t" | HighEntryBit ->"w" | LowestBit ->"y" | BitSelector ->"x"

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments = 

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

    if (Array.length arguments != 1) then (show_help (); exit 1);

	let n = int_of_string arguments.(0) in

	add FinalCycle 1 1 [FinalCycle];
	add StartEven 2 0 [FinalSink; CycleAccessDistributer];
	add StartSelector 3 0 [BitSelector; StartEven];
	add CycleAccessDistributer 4 0 (mkli n (fun j -> CycleAccess j));
	add LowestBit 5 0 [HighEntryBit; CycleSelector 0];
	add HighEntryBit 7 0 (LowestBit::FinalSink::(mkli (n - 1) (fun j -> CycleSelector (j + 1))));
	add DecLaneOddRoot (24 * n + 12) 0 [StartSelector; BitSelector];
	add BitSelector (24 * n + 14) 0 [HighEntryBit; LowestBit];
	add FinalSink (26 * n + 16) 1 [FinalCycle];

	for i = 0 to 6 * n do
		add (DecLaneEven i) (12 * n + 2 * i + 10) 1 [DecLaneOdd i];
		add (DecLaneOdd i) (12 * n + 2 * i + 9) 0 [(if i < 2 then DecLaneOddRoot else DecLaneOdd (i - 1)); BitSelector; StartEven]
	done;
	
	for i = 0 to n - 1 do
		add (CycleNode0 i) (6 * n + 6 * i + 9) 0 [CycleNode1 i; DecLaneOddRoot; CycleNodeToLane0 i];
		add (CycleNode1 i) (6 * n + 6 * i + 11) 0 [CycleNode2 i; CycleAccessDistributer; CycleNodeToLane1 i; DecLaneEven (6 * i + 4)];
		add (CycleNode2 i) (6 * n + 6 * i + 13) 0 [DecLaneEven 0; CycleCenter i; CycleNodeToLane2 i];
		add (CycleNodeToLane0 i) (6 * i + 8) 0 ((if i = 0 then [] else [CycleNodeToLane0 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 2); DecLaneEven (6 * i + 5)]);
		add (CycleNodeToLane1 i) (6 * i + 10) 0 ((if i = 0 then [] else [CycleNodeToLane1 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 1)] @ (if i < n - 1 then [DecLaneEven (6 * i + 4)] else []));
		add (CycleNodeToLane2 i) (6 * i + 12) 0 ((if i = 0 then [] else [CycleNodeToLane2 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 3); DecLaneEven (6 * i + 6)]);
		add (CycleCenter i) (6 * n + 6 * i + 14) 1 [CycleNode0 i; CycleLeaver i];
		add (CycleLeaver i) (24 * n + 4 * i + 18) 1 [UpperSelector i];
		add (UpperSelector i) (24 * n + 4 * i + 15) 0 (if i = n - 1 then [FinalSink] else [FinalSink; UpperSelectorHelper i]);
        add (CycleAccess i) (24 * n + 4 * i + 17) 1 [CycleCenter i];
        add (CycleSelector i) (6 * i + 11) 0 [CycleAccess i; UpperSelector i];
        add (CycleSelectorDecelerator i) (6 * i + 13) 0 [CycleSelector i; UpperSelector i];
		if i < n - 1 then add (UpperSelectorHelper i) (6 * i + 9) 0 (if i = n - 2 then [FinalSink; CycleSelectorDecelerator (n - 1)] else [UpperSelectorHelper (i+1); CycleSelectorDecelerator (i + 1)])
	done;

	SymbolicParityGame.to_paritygame pg;;
	
Generators.register_generator generator_game_func "expstratimpr" "Exponential Strategy Iteration Game";;