(*

nodes:				10 * n + 5
edges:				1.5 * n^2 + 20.5 * n + 6
index:				12 * n + 8
iterations voege:	9 * 2^n - 8
iterations puri:	9 * 2^n - 9

*)

(* This one breaks the cycle-avoiding rule as well as the cycle-avoiding learning rule; it is based on the Fearnley lower bound construction by adding downward escapes *)

let show_help _ =
	print_string (Info.get_title "Super-Polynomial Strategy Iteration Generator Test (Internal Use)");
	print_string ("Usage: subexptest n\n\n" ^
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
			  | CycleNodeSpecial of int * int (*b*)
			  | CycleNodeSpecialX of int * int (*l*)
			  | CycleNodeSpecialY of int * int (*p*)

let symb_to_str = function DecLaneEven i -> "a" ^ string_of_int i | DecLaneOdd i -> "t" ^ string_of_int i | DecLaneRoot -> "c" | CycleNode (i,j) -> "d(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleCenter i -> "e" ^ string_of_int i | CycleCenterBadEntry i -> "m" ^ string_of_int i | CycleCenterBadEntryX i -> "q" ^ string_of_int i | CycleNodeCho (i,j) -> "u(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleNodeSpecial (i,j) -> "b(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleNodeSpecialX (i,j) -> "l(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleNodeSpecialY (i,j) -> "p(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySel (i,j) -> "v(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySelX (i,j) -> "w(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleAccess i -> "f" ^ string_of_int i | CycleSelector i -> "g" ^ string_of_int i | CycleLeaver i -> "h" ^ string_of_int i | UpperSelector i -> "k" ^ string_of_int i | FinalSink -> "z" | FinalCycle -> "x" | StartEven -> "s" | BitSelector -> "r"  

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments =

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

    if (Array.length arguments != 1) then (show_help (); exit 1);

	let n = int_of_string arguments.(0) in
	
	add FinalCycle 1 1 [FinalCycle];
	add StartEven (10 * n + 8) 0 ( (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add DecLaneRoot (10 * n + 6) 0 [StartEven; BitSelector];
	add BitSelector (10 * n + 10) 0 ( (FinalSink::(mkli n (fun j -> CycleSelector j))));
	add FinalSink (18 * n + 12) 1 [FinalCycle];

	for i = 0 to 3 * n - 1 do
		add (DecLaneEven i) (4 * n + 2 * i + 6) 1 [DecLaneOdd i];
		add (DecLaneOdd i) (4 * n + 2 * i + 5) 0 (if i = 0 then  [DecLaneRoot; BitSelector; StartEven] else  [DecLaneOdd (i - 1); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		for j = 0 to i - 1 do
			add (CycleNodeCho (i,j)) 3 0 [CycleBadEntrySel (i,j); CycleBadEntrySelX (i,j); CycleNodeSpecial (i,j)];
			let x = (if j = 0 then CycleCenter i else CycleNodeCho (i,j-1)) in
			add (CycleNodeSpecial (i,j)) 5 0 [CycleNodeSpecialX (i,j); CycleNodeSpecialY (i,j); x];
			add (CycleNodeSpecialX (i,j)) 4 0 [CycleBadEntrySel (i,j)];
			add (CycleNodeSpecialY (i,j)) 4 0 [CycleBadEntrySelX (i,j)];
			add (CycleBadEntrySel (i,j)) 3 1 [CycleCenterBadEntry j; x];
			add (CycleBadEntrySelX (i,j)) 2 1 [CycleCenterBadEntryX j; x];
		done;
		for j = i + 1 to n - 1 do
			add (CycleNodeCho (i,j)) 3 0 [CycleBadEntrySel (i,j); CycleBadEntrySelX (i,j)];
			let x = (if j = i+1 then (if i = 0 then CycleCenter i else CycleNodeCho (i,j-2)) else CycleNodeCho (i,j-1)) in
			add (CycleBadEntrySel (i,j)) 3 1 [CycleCenterBadEntry j; x];
			add (CycleBadEntrySelX (i,j)) 2 1 [CycleCenterBadEntryX j; x];
		done;
		add (CycleNode (i,1)) (4 * i + 3) 0 ( ([StartEven; CycleNode (i,0)] @ (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleNode (i,0)) (4 * i + 3) 0 ( ([StartEven; (if n = 1 then CycleCenter i else if i = n-1 then CycleNodeCho(i,n-2) else CycleNodeCho (i,n-1))] @
		                                       (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleCenter i) (4 * i + 6) 1 [CycleNode (i,1); CycleLeaver i];
		add (CycleCenterBadEntry i) (10 * n + 11 + 2 * i) 1 [CycleCenter i];
		add (CycleCenterBadEntryX i) (12 * n + 6 * i + 15) 1 [CycleLeaver i];
		add (CycleLeaver i) (12 * n + 6 * i + 16) 1 [UpperSelector i];
		add (UpperSelector i) (12 * n + 6 * i + 11) 0 ((FinalSink::(mkli (n - i - 1) (fun j -> CycleSelector (n - j - 1)))));
        add (CycleAccess i) (12 * n + 6 * i + 13) 1 [CycleCenter i];
        add (CycleSelector i) (4 * i + 8) 0 [CycleAccess i; UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;

Generators.register_generator generator_game_func "subexpfearnley" "Super-polynomial Strategy Iteration Fearnley Game";;