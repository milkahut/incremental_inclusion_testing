open Arg;;
open Paritygame;;
open Tcsargs;;

type gamenode =
	FinalCycle
|	FinalSink
|	LowSel
|	CycleExit of int
|	UpperSelector of int
|	CycleSelector of int
|	CycleCenter of int
|	CycleNode of int * int
|	CycleEntry of int

let symb_to_str = function 
|	CycleCenter i -> "e" ^ string_of_int i
|	CycleNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int k ^ ")"
|	CycleExit i -> "y" ^ string_of_int i
|	CycleEntry i -> "q" ^ string_of_int i
|	CycleSelector i -> "u" ^ string_of_int i
|	UpperSelector i -> "w" ^ string_of_int i
|	FinalSink -> "s"
|	FinalCycle -> "t"
|	LowSel -> "p"

let generator_game_func _ =
		
	let header = Info.get_title "Exponential Round Robin Lower Bound Generator" in
	let n = ref 0 in
	  
	SimpleArgs.parsedef [] (fun s -> n := int_of_string s) (header ^ "Usage: exproundrobin [n]\n" ^
                                           "Generates the [n]-th game.\n\n");

    let n = !n in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let addnode sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	addnode FinalCycle 1 1 [FinalCycle];
	addnode FinalSink (2 * n + 10) 1 [FinalCycle];
	addnode (UpperSelector n) 3 1 [FinalSink];
	addnode (CycleSelector n) 3 1 [FinalSink];
	addnode LowSel 8 1 [CycleEntry 0];
	
	for i = 0 to n - 1 do
		addnode (CycleExit i) (2 * i + 10) 1 [UpperSelector (i + 1)];
		addnode (CycleEntry i) (2 * i + 9) 1 [CycleCenter i];
		addnode (CycleCenter i) 6 1 [CycleNode (i,0); CycleExit i];
		if (i > 0) then (
			addnode (UpperSelector i) 3 0 [UpperSelector (i+1); CycleEntry i];
			addnode (CycleSelector i) 3 0 [CycleSelector (i+1); CycleEntry i];
		);
		for j = 0 to i do
			addnode (CycleNode (i,j)) 5 0 [CycleSelector 1; FinalSink; LowSel; (if j < i then CycleNode (i,j+1) else CycleCenter i)]
		done;
	done;

	SymbolicParityGame.to_paritygame pg;;

Generators.register_generator generator_game_func "exproundrobin" "Exponential Round Robin Lower Bound Generator";;