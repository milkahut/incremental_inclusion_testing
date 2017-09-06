open Paritygame;;
open Tcsset;;

type global_solver_factory = string array -> global_solver

let solvermap = ref StringMap.empty;;

let register_solver_factory solver_func identifier abbreviation description =
	if StringMap.mem identifier !solvermap
	then failwith ("Solver `" ^ identifier ^ "' already registered!\n")
	else solvermap := StringMap.add identifier (solver_func, abbreviation, description) !solvermap;;
	
let register_solver solver_func = register_solver_factory (fun _ -> solver_func);;

let mem_solver identifier = StringMap.mem identifier !solvermap;;

let find_solver identifier = StringMap.find identifier !solvermap;;

let enum_solvers it = StringMap.iter (fun i (f, a, d) -> it f i a d) !solvermap;;

let fold_solvers fo b = StringMap.fold (fun i (f, a, d) x -> fo f i a d x) !solvermap b;;


type partial_solver_factory = string array -> partial_solver

let partialsolvermap = ref StringMap.empty;;

let register_partial_solver_factory solver_func identifier abbreviation description =
	if StringMap.mem identifier !partialsolvermap
	then failwith ("Partial Solver `" ^ identifier ^ "' already registered!\n")
	else partialsolvermap := StringMap.add identifier (solver_func, abbreviation, description) !partialsolvermap;;
	
let register_partial_solver solver_func = register_partial_solver_factory (fun _ -> solver_func);;

let mem_partial_solver identifier = StringMap.mem identifier !partialsolvermap;;

let find_partial_solver identifier = StringMap.find identifier !partialsolvermap;;

let enum_partial_solvers it = StringMap.iter (fun i (f, a, d) -> it f i a d) !partialsolvermap;;

let fold_partial_solvers fo b = StringMap.fold (fun i (f, a, d) x -> fo f i a d x) !partialsolvermap b;;