open Benchmarking
open Arg
open Nba

(* PARAMETERS *)

let timeout = ref(60.)
let with_pebble = ref(false)
let with_static = ref(false)
let with_dynamic = ref(false)

let tool_name = Sys.argv.(0)

let commands = align
  [
   "-timeout", Set_float(timeout), " Sets timeout in seconds (default=60.)";
   "-pebble", Set(with_pebble), " Checks inclusion with multi-pebble simulations";
   "-static", Set(with_static), " Checks inclusion with static simulations";
   "-dynamic", Set(with_dynamic), " Checks inclusion with dynamic simulations";
  ] 

let help_message =
  tool_name ^ ": proves language inclusion of two NBA by incremental search of a simulation.\n" 
  ^ "Usage: " ^ tool_name ^ " [options] file1 file2\n" 
  ^ "Supported file extensions: .ba (Rabit), .gff (Goal).\n" 
  ^ "Supported options: try " ^tool_name ^ " -help\n"

(* OPEN AUT *)
let open_aut fname = 
  if fname.[(String.length fname)-1] = 'a' 
  then nba_of_ba_file fname
  else nba_of_gff_file fname
      
(* CHECK *)

let check a1 a2 meth meth_name =
  Format.printf "Starts proving inclusion with %s...@." meth_name;
  let res = meth a1 a2 !timeout no_result in
  match inclusion_status res, construction_time res, nb_iterations res 
  with
  | Some(true),Some(t),Some(k) ->  
      Format.printf "done \\o/@.time needed: %a@.number of iterations needed: %i@." Misc.pp_time t k
  | _ -> 
      Format.printf "time out, sorry. Try option -timeout.@." 

 

					     
(* MAIN *)
let _ = 
  let l = ref [] in
  parse commands (fun s -> l:=s:: !l) help_message;
  let file1,file2 = match !l with
  | [file2;file1] -> file1,file2
  | _ -> usage commands help_message; exit 0 in
  let a1,a2 = open_aut file1 , open_aut file2 in
  let a1,a2 = normalize_pair (a1,a2) in
  if !with_pebble then check a1 a2 m_pebble "pebble";
  if !with_static then check a1 a2 m_static "static";
  if !with_dynamic then check a1 a2 m_dynamic "dynamic";


