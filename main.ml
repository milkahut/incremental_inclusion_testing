open Game;;
open Nba;;
open Libpgsolver;;
open Printf;;
open Nba_examples;;
open Paritygame;;
open Arg;;
open Benchmarks;;

module LocalStaticGame   = Game.MakeGame(Staticgame.Local);;
module GlobalStaticGame  = Game.MakeGame(Staticgame.Global);;
module LocalDynamicGame  = Game.MakeGame(Dynamicgame.Local);;
module GlobalDynamicGame = Game.MakeGame(Dynamicgame.Global);;
module LocalPebbleGame   = Game.MakeGame(Pebblegame.Local);;
module GlobalPebbleGame  = Game.MakeGame(Pebblegame.Global);;


type stop = NotAtAll
          | Fixed of int
          | Exists
	  | All

type statistics = (int * float * float * int) array

let _ =
  Random.self_init ();

  let static = ref false in
  let dynamic = ref false in
  let pebble = ref false in
  let start = ref 1 in
  let stop = ref NotAtAll in
  let iterations = ref 1 in
  let callgoal = ref false in
  let savegff = ref false in
  let global = ref false in

  let commands = [ ("-static", Set(static), " , run tests with the static game");
                   ("-dynamic", Set(dynamic), ", run tests with the dynamic game");
                   ("-pebble",  Set(pebble),  " , run tests with the multi-pebble game");
		   ("-callgoal", Set(callgoal), " , call goal to decide language inclusion");
		   ("-savegff", Set(savegff), " , save the considered automata as Ai.gff and Bi.gff");
                   ("-global", Set(global), " , create the global games instead of the local ones");
                   ("-init", Set_int(start), "<k> , set initial value for parameter k, default is 1");
                   ("-last", Int(fun i -> stop := Fixed(i)), "<k> , set last value for parameter k, default is unbounded");
                   ("-exists", Unit(fun _ -> stop := Exists), ", stop as soon as player 0 wins some game");
                   ("-all", Unit(fun _ -> stop := All), "   , stop when player 0 wins all games");
                   ("-iter", Set_int(iterations), "<i> , set number of iterations per test") ]
  in
  let help_message = "incl2pg: check Büchi inclusion through parity game solving \n\n"
                   ^ "Usage: incl2pg [options] <benchmark> [parameters]\n\n"
                   ^ "Input     : NBAs A and B, a positive k\n"
                   ^ "Constructs: parity game(s) G(A,B,k) that are won by player 0 if L(A) is included in L(B)\n"
                   ^ "(The converse direction may not be true).\n\n"
                   ^ "Output format: |G(A,B,k)| / building time / solving time / winner of initial node\n\n"
		   ^ "Available benchmarks: " ^ (String.concat ", " (list_all_benchmarks ())) ^ "\n\n"
  in
  let args_aux = ref [] in
  parse commands (fun s -> args_aux := s :: !args_aux) help_message;
  let args_aux = List.rev !args_aux in
  print_string "Chosen benchmark ............................... ";
  if args_aux=[] then (print_string ("none! That's not good ...\n\n" ^ help_message); exit 0);
  let benchmark = get_benchmark (List.hd args_aux) (Array.of_list (List.tl args_aux)) in
  print_string (List.hd args_aux);
  print_newline ();

  print_string "Trying to find recursive parity game solver .... ";
  let (solver_fac,_,_) = find_solver "recursive" in
  let solver = solver_fac [||] in
  print_string "found!";
  print_newline ();

  let call_goal auta autb =
    printf "calling goal...   ";
    try
      let res = Gff_tools.language_inclusion auta autb in
      printf "done. %sncluded.\n" 
	(if res then "I" else "Not i")
    with Gff_tools.Goal_failure str-> printf "failed.\n%s\n" str
  in

  let check auta autb k =
    let stats = Array.make 3 (-1,-1,-1.,-1.,-1) in
    let testcase game name i =
      printf "  %s game %s%!" name (String.make (17 - (String.length name)) '.');
      let f = Sys.time () in
      let (g,_) = game auta autb k in
      let build_time = (Sys.time ()) -. f in
      let nodes = pg_node_count g in
      let edges = pg_edge_count g in
(*      print_game g; *)
      printf "%7i / %8i / %6.2fs%!" nodes edges build_time;
      let f = Sys.time () in
      let (sol,_) = solver g in
      let solve_time = (Sys.time ()) -. f in
      stats.(i) <- (nodes, edges, build_time, solve_time, sol.(0));
      printf " / %6.2fs / %i" solve_time sol.(0);
      print_newline ()
    in
    if !static  then testcase (if !global then GlobalStaticGame.game else LocalStaticGame.game) "static" 0;
    if !dynamic then testcase (if !global then GlobalDynamicGame.game else LocalDynamicGame.game) "dynamic" 1;
    if !pebble  then testcase (if !global then GlobalPebbleGame.game else LocalPebbleGame.game) "pebble" 2;
    stats
  in
  

  let k = ref !start in
  let continue = ref (match !stop with Fixed i -> !k <= i
                                     | _       -> true)
  in
  while !continue do 
    print_string ("Starting outer iteration with k = " ^ string_of_int !k);
    print_newline ();
    for i = 1 to !iterations do
      print_string (" Starting inner iteration with i = " ^ string_of_int i);
      print_newline ();
      let (auta, autb) = benchmark i in
      if !k= !start then begin 
	let auta,autb = get_nba auta, get_nba autb in
	  if !callgoal then call_goal auta autb;
	  if !savegff then begin
	    let fname str = str ^(string_of_int i)^".gff" in
	    Gff_tools.gff_file_of_aut_array auta (fname "A");
	    Gff_tools.gff_file_of_aut_array autb (fname "B")
	  end
	end;
      let stats = check auta autb !k in
      continue := match !stop with Fixed i  -> !k < i
                                 | NotAtAll -> true
 	  	                 | Exists   -> not (Array.fold_left (fun b -> fun (_,_,_,_,w) -> b || w=0) false stats)
		  	         | All      -> not (Array.fold_left (fun b -> fun (_,_,_,_,w) -> b && w <= 0) true stats)
    done;
    incr k;
  done
