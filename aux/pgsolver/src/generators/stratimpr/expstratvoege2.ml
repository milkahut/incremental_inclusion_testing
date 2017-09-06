let show_help _ =
	print_string (Info.get_title "Exponential Strategy Iteration Generator Voege");
	print_string ("Usage: expstratvoege2 n\n\n" ^
	              "       where n = n-th exponential strategy iteration game\n" ^
	              "Notes: - If you want to use it directly with PGSolver, you need to\n" ^
	              "           - either disable scc decomposition, global and local optimization,\n" ^
	              "             i.e. -dsd -dgo -dlo\n" ^
	              "           - or transform the game using the transformer tool as follows:\n" ^
	              "             transformer -ss -ap -bn\n" ^
	              "\n")

open Paritygame;;
open Tcslist;;

type gamenode = ResetPoint (* r *)
			  | StartPoint (* s *)
			  | DecLaneInternal of int (* b *)
			  | DecLaneEntry of int (* a *)
			  | BoundedSelEntry of int * int (* f *)
			  | SelLaneInternal of int (* t *)
			  | CycleInternal of int (* e *)
			  | CycleEntry of int (* h *)
			  | GateEntry of int (* m *)
			  | GateRoot of int (* k *)
			  | GateSelector of int (* l *)
			  | UpperSelector of int (* z *)
			  | FSink (* q *)
			  | Sink (* p *)

let symb_to_str = function
				ResetPoint -> "r"
			  | StartPoint -> "s"
			  | DecLaneInternal i -> "b" ^ string_of_int i
			  | DecLaneEntry i -> "a" ^ string_of_int i
			  | BoundedSelEntry (i,j) -> "f(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")"
			  | SelLaneInternal i -> "t" ^ string_of_int i
			  | CycleInternal i -> "e" ^ string_of_int i
			  | CycleEntry i -> "h" ^ string_of_int i
			  | GateEntry i -> "m" ^ string_of_int i
			  | GateRoot i -> "k" ^ string_of_int i
			  | GateSelector i -> "l" ^ string_of_int i
			  | UpperSelector i -> "z" ^ string_of_int i
			  | FSink -> "q"
			  | Sink -> "p"
			  
(* low_prio odd, entry_prio odd *)			  
let build_dec_lane add roots length low_prio entry_prio =	
	add (DecLaneEntry 0) entry_prio 1 [DecLaneInternal 0];
	add (DecLaneInternal 0) (entry_prio + 1) 0 (FSink::roots);
	for i = 1 to length - 1 do
		add (DecLaneInternal i) (low_prio + 2 * i - 2) 0 (FSink::DecLaneInternal (i-1)::roots);
		add (DecLaneEntry i) (low_prio + 2 * i - 1) 1 [DecLaneInternal i]
	done

(* prio even *)	
let build_bounded_selector add roots selections length prio idx =
	for i = 0 to length - 1 do
		add (BoundedSelEntry (idx,i)) prio 0 (if i = 0 then FSink::(selections i)::roots else (FSink::BoundedSelEntry (idx,i-1)::(selections i)::roots))
	done

(* low_prio even *)
let build_selector_lane add roots selectors sel_length low_prio =
	let base_prio = low_prio + selectors * sel_length * 2 in
	ListUtils.iteri (fun i root ->
		add (SelLaneInternal i) base_prio 1 [root]
	) roots;
	let selroots = ListUtils.init (List.length roots) (fun i -> SelLaneInternal i) in
	build_dec_lane add roots (selectors * sel_length) (low_prio + 1) (base_prio + 1);
	for i = 0 to selectors - 1 do
		build_bounded_selector add selroots (fun j -> DecLaneEntry (i + j * selectors)) sel_length low_prio i
	done

(* low_prio odd *)			 
let build_cycle add targets low_prio idx =
	add (CycleInternal idx) low_prio 0 (FSink::(CycleEntry idx)::targets);
	add (CycleEntry idx) (low_prio + 1) 1 [CycleInternal idx; GateEntry idx]

(* low_prio odd, gate_prio odd *)
let build_gate add center exit low_prio gate_prio idx =
	add (GateRoot idx) gate_prio 1 [center];
	add (GateEntry idx) (gate_prio + 1) 1 [exit];
	add (GateSelector idx) low_prio 0 [FSink; exit; GateRoot idx]

(* low_prio odd, gate_prio odd, low_prio_gate odd *)
let build_cycled_gate add targets exit low_prio_cycle low_prio_gate gate_prio idx =
	build_cycle add targets low_prio_cycle idx;
	build_gate add (CycleEntry idx) exit low_prio_gate gate_prio idx
			  
			  
let generator_game_func arguments = 

	let pg = SymbolicParityGame.create_new Sink in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

    if (Array.length arguments != 1) then (show_help (); exit 1);

	let n = int_of_string arguments.(0) in

	add Sink 1 1 [Sink];
	add FSink (20 * n + 40) 1 [Sink];
	add StartPoint (20 + 8 * n) 0 (FSink::ListUtils.init n (fun i -> GateSelector i));
	add ResetPoint (18 + 8 * n) 0 (FSink::ListUtils.init n (fun i -> GateRoot i));
	build_selector_lane add [ResetPoint;StartPoint] 2 (2 * n) 4;
	for i = 0 to n - 1 do
		add (UpperSelector i) 2 0 (if i = n - 1 then [FSink] else [FSink;UpperSelector (i+1); GateSelector (i+1)]);
		build_cycled_gate add [ResetPoint;StartPoint;BoundedSelEntry (0,n+i);BoundedSelEntry (1,n+i)] (UpperSelector i) 3 3 (20 + 8 * n + 1 + 2 * i) i
	done;
	
	SymbolicParityGame.to_paritygame pg;;
	
Generators.register_generator generator_game_func "expstratvoege2" "Exponential Strategy Iteration Voege";;
