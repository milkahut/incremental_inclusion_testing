open Printf
open Unix


(******* BENCHMARKS ******)

type benchmark_identifier = {
    bi_name: string; (** name of the (family of) benchmark *)
    bi_param: int option; (** parameter of the benchmark (e.g. : size) *)
    bi_variant: int option; (** variant of the benchmark *)
  }

(* path of the root dir of benchmarks *)
let bench_path_root = "../benchmarks/"


(* dir associated to a bench *)
let bench_dir bid = 
  bid.bi_name ^
  (match bid.bi_param with
  | None -> ""
  | Some(i)->string_of_int i) ^
  (match bid.bi_variant with
  | None -> ""
  | Some(i)->"_" ^ (string_of_int i))


let bench_path bid = 
  Format.sprintf "%s%s/" bench_path_root (bench_dir bid)

let load_benchmark bid = 
  let auta = Nba.nba_of_gff_file ((bench_path bid)^"A.gff") in
  let autb = Nba.nba_of_gff_file ((bench_path bid)^"B.gff") in
  (auta,autb)

let save_benchmark bid auta autb =
  Nba.gff_file_of_nba auta ((bench_dir bid)^"A.gff");
  Nba.gff_file_of_nba autb ((bench_dir bid)^"B.gff");


(******* RESULTS ******)

type result_kind = { rk_id: string;rk_desc:string}
let rk_inclusion = { rk_id= "inclusion";rk_desc="inclusion"}
let rk_iteration = { rk_id= "n_iterations";rk_desc="k"}
let rk_nodes = { rk_id="n_nodes";rk_desc="number of nodes of the game"}
let rk_transitions = { rk_id="n_trans";rk_desc="number of transitions of the game"}
let rk_construction_time = { rk_id="construction_time";rk_desc="construction time"}
let rk_solving_time = { rk_id="solving_time";rk_desc="solving time"}
let rk_total_time = { rk_id="total_time";rk_desc="time"}
let all_result_kinds = 
  [rk_inclusion;rk_iteration;rk_nodes;rk_transitions;
   rk_construction_time;rk_solving_time;rk_total_time]

type result_value = string
type result = (result_kind * result_value) list

let no_result = []

let get_rk rk result = snd (List.find (fun (rk2,_)->rk=rk2) result)

let inclusion_status result =
  try Some(bool_of_string(get_rk rk_inclusion result))
  with Not_found -> None

let construction_time result =
  try Some(float_of_string(get_rk rk_total_time result))
  with Not_found -> None

let nb_iterations result = 
  try Some(int_of_string(get_rk rk_iteration result))
  with Not_found -> None

let pp_result fmt result = 
  let inclusion = "inclusion: "^(
    try get_rk rk_inclusion result
    with Not_found -> "don't know") in
  let time fmt = 
    Format.fprintf fmt "time needed: ";
    try 
      Misc.pp_time fmt (float_of_string (get_rk rk_total_time result))
    with Not_found -> Format.fprintf fmt "undefined" in
  let iteration = "iterations: " ^ (
    try get_rk rk_iteration result 
    with Not_found -> "undefined") in
  Format.fprintf fmt "%s, %t, %s"
    inclusion time iteration    


type method_name = string

let get_res bid method_name =
  let fname = (bench_path bid)^method_name^".res" in
  let ch = ref (None) in
  let res = ref([]) in  
  let rk_of_string str = 
    List.find (fun rk->rk.rk_id=str) all_result_kinds in
  let rec parse ch =
    let rk = rk_of_string (input_line ch) in
    res:=(rk,(input_line ch))::!res;
    parse ch in
  try
    let ch2 = open_in fname in
    ch := Some(ch2);
    ignore (parse ch2);
    assert false
  with
    _ -> begin
      begin match !ch with
      | Some(ch2) -> close_in ch2
      | None -> () 
      end;
      !res
    end

let write_res bid method_name res  =
  let fname = (bench_path bid)^method_name^".res" in
  let ch = open_out fname in
  let fmt = Format.formatter_of_out_channel ch in
  let f (rk,rval) =
    Format.fprintf fmt "%s@.%s@." rk.rk_id rval in
  List.iter f res;
  close_out ch



type timeout = float

type method_t =  Nba.nba->Nba.nba->timeout->result->result

let result_of incl k n_nodes n_tr ct st tt = 
  [rk_inclusion,string_of_bool incl;
   rk_iteration,string_of_int k;
   rk_nodes,string_of_int n_nodes;
   rk_transitions,string_of_int n_tr;
   rk_construction_time,string_of_float ct;
   rk_solving_time,string_of_float st;
   rk_total_time,string_of_float tt]

let (solver_fac,_,_) = Libpgsolver.find_solver "recursive" 
let solver = solver_fac [||] 

let pg_solver = ref (fun g->(fst (solver g)).(0) = 0)


let solve_with game auta autb timeout previous_result =
  if List.exists (fun (rk,_)->rk=rk_inclusion) previous_result
  then previous_result (* the inclusion has been already proved *)
  else 
    let total_time = ref (0.) in
    let result = ref(previous_result) in
    let k = ref (1) in
    let continue = ref (true) in
    try while (!total_time<timeout) && !continue do
      let (g,ct) = 
	Misc.with_time_out timeout (fun () ->game auta autb !k) in
      let ((inclusion),st) = Misc.with_time_out timeout (fun ()-> (!pg_solver) g) in
      let n_nodes,n_tr = 
	(Paritygame.pg_node_count g),(Paritygame.pg_edge_count g) in
      total_time:=!total_time+. ct +. st;
      result := result_of inclusion !k n_nodes n_tr ct st !total_time;
      k:=!k+1;
      continue := not inclusion
    done; !result  
    with _ -> !result

module StaticGame = Game.MakeGame(Staticgame.Local)
module DynamicGame = Game.MakeGame(Dynamicgame.Local)
module PebbleGame = Game.MakeGame(Pebblegame.Local)

let game_of f = fun auta->fun autb->fun k->fst (f auta autb k)

let m_static = solve_with (game_of StaticGame.game)
let m_dynamic = solve_with (game_of DynamicGame.game)
let m_pebble = solve_with (game_of PebbleGame.game)


let _ = Random.self_init () 
(* needed to generate "fresh" names for temporary files *)

let m_arbait auta autb timeout previous_result = 
  if List.exists (fun (rk,_)->rk=rk_inclusion) previous_result
  then previous_result (* the inclusion has been already proved *)
  else begin
    let cwd = Sys.getcwd () in
    let i = string_of_int (Random.bits ()) in
    let fname str = cwd ^ "/" ^ i ^ str ^ "_tmp.ba" in
    let (f_auta,f_autb) = (fname "A"), (fname "B") in
    Nba.ba_file_of_nba auta f_auta; 
    Nba.ba_file_of_nba autb f_autb;     
    let lines = 
      Misc.shell_with_time_out timeout
	("java -jar CheckingInclusion_SimulationSubsumption.jar " 
	 ^ f_auta ^" "^ f_autb
	 ^" -q -b -rd -fplus -SFS -qr -c -l") 	 

    in
    let _cleaning = 
      system (Format.sprintf "rm %s %s" (fname "A") (fname "B")) in
    let is_included = 
      if List.exists (fun str->str="Not Included") lines
      then [rk_inclusion,"false"]
      else if List.exists (fun str->str="Included") lines
      then [rk_inclusion,"true"]
      else [] in
    let time = 
      try
	let str = List.find (fun str->str.[0]='T') lines in
	let i = String.index str ':' in
	let str2 = String.sub str (i+1) ((String.length str) -i -1) in
	let time_in_sec = (float_of_string str2) /. 1000. in
	[rk_total_time,string_of_float time_in_sec]
      with Not_found -> [rk_total_time,string_of_float timeout] in
    is_included@time
  end 


