open Arg;;
open Paritygame;;
open Tcsargs;;
open Tcsmaths;;

type gamenode =
	FinalCycle
|	FinalSink
|	CycleExit of int
|	UpperSelector of int
|	CycleSelector of int
|	CycleCenter of int
|	CycleNode of int * int
|	IntermediateNode of int * int
|	CycleEntry of int
|	AccessCenter of int
|	AccessNode of int * int
|	AccessExit of int
|	UpperRoot
|	SelectorRoot

let symb_to_str = function 
|	CycleCenter i -> "e" ^ string_of_int i
|	CycleNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int 0 ^ "," ^ string_of_int k ^ ")"
|	IntermediateNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int 1 ^ "," ^ string_of_int k ^ ")"
|	_ -> ""

let mkli n f = if n < 1 then [] else (Array.to_list (Array.init n f))

let mkli2 i n f = mkli (n - i) (fun j -> f (i + j))

let mkli_wo n i f = mkli (n - 1) (fun j -> if j < i then f j else f (i + 1))

let binlog n = int_of_float (ceil ((log (float_of_int n)) /. (log 2.)))

let with_strategy = false

let generator_game_func _ =
		
	let header = Info.get_title "Subexponential Random Facet Lower Bound Generator" in
	let n = ref 0 in
	  
	SimpleArgs.parsedef [] (fun s -> n := int_of_string s) (header ^ "Usage: subexprandomedge [n]\n" ^
                                           "Generates the [n]-th game.\n\n");

    let n = !n in
    
	let access_cycle = 2 * n in
	let intermediate_cycle = 4 * n in

    let counting_cycle i = (i + 2) * n * 4 in
    		  
	let pg = SymbolicParityGame.create_new FinalCycle in
	
	let addnodenormal sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	let addnodestrat sy pr pl li strat =
		SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy ^ "[" ^ symb_to_str strat ^ "]")) in

	let addnode sy pr pl li strat =
		if with_strategy
		then addnodestrat sy pr pl li strat
		else addnodenormal sy pr pl li
	in
	
	addnodenormal FinalCycle 1 1 [FinalCycle];
	addnodenormal FinalSink (4 * n + 10) 1 [FinalCycle];
	addnode (UpperSelector n) 3 0 [FinalSink] FinalSink;
	addnode (CycleSelector n) 3 0 [FinalSink] FinalSink;
	addnodenormal UpperRoot 8 1 [UpperSelector 0];
	addnodenormal SelectorRoot 3 1 [CycleSelector 0];
	
	for i = 0 to n - 1 do
		addnodenormal (CycleExit i) (4 * i + 12) 1 [UpperSelector (i + 1)];
		addnodenormal (AccessExit i) (4 * i + 9) 1 [SelectorRoot];
		addnodenormal (CycleEntry i) (4 * i + 11) 1 [CycleCenter i];
		addnodenormal (AccessCenter i) 6 1 [CycleEntry i; AccessNode (i,0)];
		addnodenormal (CycleCenter i) 6 1 [CycleNode (i,0); (*IntermediateNode (i,0);*) CycleExit i];
		addnode (UpperSelector i) 3 0 [UpperSelector (i+1); FinalSink; AccessCenter i] FinalSink;
		addnode (CycleSelector i) 3 0 [CycleSelector (i+1); FinalSink; CycleEntry i] FinalSink;
		for j = 0 to counting_cycle i - 1 do
			addnode (CycleNode (i,j)) 5 0 [SelectorRoot; FinalSink; (if j < counting_cycle i - 1 then CycleNode (i,j+1) else IntermediateNode (i,0) (*CycleCenter i*))] FinalSink
		done;
		for j = 0 to intermediate_cycle - 1 do
			addnode (IntermediateNode (i,j)) 5 0 [UpperRoot; FinalSink; (if j < intermediate_cycle - 1 then IntermediateNode (i,j+1) else CycleCenter i)] FinalSink
		done;
		for j = 0 to access_cycle - 1 do
			addnode (AccessNode (i,j)) 5 0 [AccessExit i; FinalSink; (if j < access_cycle - 1 then AccessNode (i,j+1) else AccessCenter i)] FinalSink
		done;
	done;

	SymbolicParityGame.to_paritygame pg;;

Generators.register_generator generator_game_func "subexprandomedge" "Subexponential Random Facet Lower Bound Generator";;