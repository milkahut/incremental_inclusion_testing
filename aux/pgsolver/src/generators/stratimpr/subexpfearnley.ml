(*

nodes:				10 * n + 5
edges:				1.5 * n^2 + 20.5 * n + 6
index:				12 * n + 8
iterations voege:	9 * 2^n - 8
iterations puri:	9 * 2^n - 9

*)

let show_help _ =
	print_string (Info.get_title "Super-Polynomial Strategy Iteration Generator Fearnley");
	print_string ("Usage: subexpfearnley n\n\n" ^
	              "       where n = n-th super-polynomial strategy iteration game\n" ^
	              "Notes: - If you want to use it directly with PGSolver, you need to\n" ^
	              "           - either disable scc decomposition, global and local optimization,\n" ^
	              "             i.e. -dsd -dgo -dlo\n" ^
	              "           - or transform the game using the transformer tool as follows:\n" ^
	              "             transformer -ss -ap -bn\n" ^
	              "\n")

open Paritygame;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*t*)
			  | DecLaneRoot (*c*)
			  | CycleNode of int * int (*d*)
			  | CycleCenter of int (*e*)
			  | CycleCenterBadEntry of int (*m*)
			  | CycleCenterBadEntryX of int (*q*)
			  | CycleNodeCho of int * int (*u*)
			  | CycleBadEntrySel of int * int (*v*)
			  | CycleBadEntrySelX of int * int (*w*)
			  | CycleAccess of int (*f*)
			  | CycleSelector of int (*g*)
			  | CycleLeaver of int (*h*)
			  | UpperSelector of int (*k*)
			  | FinalSink (*z*)
			  | FinalCycle (*x*)
			  | BitSelector (*r*)
			  | StartEven (*s*)

let symb_to_str = function DecLaneEven i -> "a" ^ string_of_int i | DecLaneOdd i -> "t" ^ string_of_int i | DecLaneRoot -> "c" | CycleNode (i,j) -> "d(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleCenter i -> "e" ^ string_of_int i | CycleCenterBadEntry i -> "m" ^ string_of_int i | CycleCenterBadEntryX i -> "q" ^ string_of_int i | CycleNodeCho (i,j) -> "u(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySel (i,j) -> "v(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySelX (i,j) -> "w(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleAccess i -> "f" ^ string_of_int i | CycleSelector i -> "g" ^ string_of_int i | CycleLeaver i -> "h" ^ string_of_int i | UpperSelector i -> "k" ^ string_of_int i | FinalSink -> "z" | FinalCycle -> "x" | StartEven -> "s" | BitSelector -> "r"  

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments =

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

    if (Array.length arguments < 1) || (Array.length arguments > 2) then (show_help (); exit 1);

	let n = int_of_string arguments.(0) in
	
	let withfearnley = Array.length arguments = 1 in

	add FinalCycle 1 1 [FinalCycle];
	add StartEven (10 * n + 6) 0 ( (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add DecLaneRoot (10 * n + 4) 0 [StartEven; BitSelector];
	add BitSelector (10 * n + 8) 0 ( (FinalSink::(mkli n (fun j -> CycleSelector j))));
	add FinalSink (18 * n + 10) 1 [FinalCycle];

	for i = 0 to 3 * n - 1 do
		add (DecLaneEven i) (4 * n + 2 * i + 4) 1 [DecLaneOdd i];
		add (DecLaneOdd i) (4 * n + 2 * i + 3) 0 (if i = 0 then  [DecLaneRoot; BitSelector; StartEven] else  [DecLaneOdd (i - 1); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		if withfearnley then (
			for j = i + 1 to n - 1 do
				add (CycleNodeCho (i,j)) 3 0 [CycleBadEntrySel (i,j); CycleBadEntrySelX (i,j)];
				add (CycleBadEntrySel (i,j)) 3 1 [CycleCenterBadEntry j; (if j = i+1 then CycleCenter i else CycleNodeCho (i,j-1))];
				add (CycleBadEntrySelX (i,j)) 2 1 [CycleCenterBadEntryX j; (if j = i+1 then CycleCenter i else CycleNodeCho (i,j-1))];
			done;
		);
		add (CycleNode (i,1)) (4 * i + 3) 0 ( ([StartEven; CycleNode (i,0)] @ (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleNode (i,0)) (4 * i + 3) 0 ( ([StartEven; (if (i = n-1) || not withfearnley then CycleCenter i else CycleNodeCho (i,n-1))] @ (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleCenter i) (4 * i + 4) 1 [CycleNode (i,1); CycleLeaver i];
		if withfearnley then (
			add (CycleCenterBadEntry i) (10 * n + 9 + 2 * i) 1 [CycleCenter i];
			add (CycleCenterBadEntryX i) (12 * n + 6 * i + 13) 1 [CycleLeaver i];
		);
		add (CycleLeaver i) (12 * n + 6 * i + 14) 1 [UpperSelector i];
		add (UpperSelector i) (12 * n + 6 * i + 9) 0 ((FinalSink::(mkli (n - i - 1) (fun j -> CycleSelector (n - j - 1)))));
        add (CycleAccess i) (12 * n + 6 * i + 11) 1 [CycleCenter i];
        add (CycleSelector i) (4 * i + 6) 0 [CycleAccess i; UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;

Generators.register_generator generator_game_func "subexpfearnley" "Super-polynomial Strategy Iteration Fearnley Game";;