let _ = Random.self_init ()

let goal_absolute = Gff_tools.goal_absolute ()

let bench_root_absolute_path  =
  (Sys.getcwd()) ^ "/../benchmarks/"

let list_of_pairs l =
  let f x y = if x=y then [] else [x,y] in
  let g l x = List.flatten (List.map (f x) l) in
  List.flatten (List.map (g l) l)
  
let command cmdline = 
  Format.printf "@.%s@." cmdline;
  ignore (Sys.command cmdline)

let mkdir dir =
  command (Format.sprintf "mkdir %s%s" bench_root_absolute_path dir)

let make_benchs_from_set_dir subdir =
  let commonpath = bench_root_absolute_path ^ subdir in
  let files = Array.to_list (Sys.readdir (commonpath ^ "/")) in
  let i = ref 0 in
  let ln i a_or_b file = 
    command (Format.sprintf "ln -s %s/%s %s%i/%s.gff"
	       commonpath file commonpath i a_or_b) in
  let f (fileA,fileB) =
    i:= !i+1;
    mkdir (Format.sprintf "%s%i" subdir !i);
    ln !i "A" fileA;
    ln !i "B" fileB in
  List.iter f (list_of_pairs files)
	

(* LTLDIFF GENERATOR *)

let ltl2ba_methods_list =
  ["tableau";
   "inctableau";
   "temporaltester";
   "gpvw";
   "gpvw+";
   "ltl2aut";
   "ltl2aut+";
   "ltl2ba";
   "pltl2ba";
   "couvreur";
   "ltl2buchi";
   "modella"]

let gen_ltldiff_benchs formula subdir =
  let f mname = 
    let cmdline = 
      Format.sprintf "%s translate -m %s -o %s%s/%s.gff \"%s\""
	goal_absolute
	mname
	bench_root_absolute_path
	subdir
	mname
	formula 
    in command cmdline in
  mkdir subdir;
  List.iter f ltl2ba_methods_list;
  make_benchs_from_set_dir subdir

(* COMPLDIFF GENERATOR *)

    
let compl_methods_list = 
  ["deterministic";
   "kurshan";
   "modifiedsafra";
   "ms";
   "piterman";
   "ramsey";
   "rank";
   "safra";
   "slice";
    "waa";
    "wapa"]

let gen_compldiff_benchs file subdir = 
  let f mname =
    let cmdline =
      Format.sprintf "%s complement -m %s -o %s%s/%s.gff %s%s"
	goal_absolute
	mname
	bench_root_absolute_path
	subdir
	mname
	bench_root_absolute_path
	file
    in command cmdline in
  mkdir subdir;
  List.iter f compl_methods_list;
  make_benchs_from_set_dir subdir


(* GENERATOR BY RANDOMIZED OBFUSCATION *)

let gen_with_obfuscator obfuscate src_dir subdir = 
  let f src_dir =
    let fname = bench_root_absolute_path ^ src_dir ^ "/A.gff" in
    let fnamea = bench_root_absolute_path ^ subdir ^ "/A.gff" in
    let fnameb = bench_root_absolute_path ^ subdir ^ "/B.gff" in
    let aut = Nba.nba_of_gff_file fname in
    let auta = obfuscate aut in
    let autb = obfuscate aut in
    mkdir subdir;
    Nba.gff_file_of_nba auta fnamea;
    Nba.gff_file_of_nba autb fnameb
  in List.iter f (Gff_tools.expand_dot_dot_notation [src_dir])

let state_split_obfuscator k p1 p2 = 
  let k = int_of_string k in
  let p1,p2 = float_of_string p1, float_of_string p2 in
  if (p2>p1) || (p1<0.) || (p1>1.) || (p2<0.) || (p2>1.) then invalid_arg "probabilities";
  (fun nba ->Nba.state_splitting nba k p1 p2) 

(* MAIN *)

type generator_desc = 
    { g_name : string;
      g_arity : int;
      g_descr : string;
      g_example : string;
      g_function : string array -> (string -> unit);
    }

let generators_list = 
  [{g_name = "ltldiff";
    g_arity = 1;
    g_descr = "use different ltl->NBA translations";
    g_example = "\"(G F p) --> G F (q /\\ X p)\"";
    g_function = (fun arg ->gen_ltldiff_benchs arg.(0))};
   {g_name = "compldiff";
    g_arity = 1;
    g_descr = "use different complementation algorithms";
    g_example = "A.gff";
    g_function = (fun arg -> gen_compldiff_benchs arg.(0))};
   {g_name = "obfuscate";
    g_arity = 4;
    g_descr = "obfuscate by splitting states k times with probabilities p1 and p2 (p2<=p1)";
    g_example = "4 0.55 0.45 src_dir1..7";
    g_function = (fun arg -> 
      let obfuscate = state_split_obfuscator arg.(0) arg.(1) arg.(2) 
      in gen_with_obfuscator obfuscate arg.(3))}
 ]
    
let pp_available_methods fmt =
  let f {g_name=name} = 
    Format.fprintf fmt "@ %s" name in
  List.iter f generators_list

let pp_descr fmt =
  let f {g_name=name;g_descr=descr} = 
    Format.fprintf fmt "@ @[* %s : %s.@]" name descr in
  List.iter f generators_list

let pp_examples fmt = 
  let f {g_name=name;g_example=example} = 
    Format.fprintf fmt "@ @[bench_gen foo_dir %s %s @]"
      name example in
  List.iter f generators_list


let inv_param () = 
  Format.fprintf Format.std_formatter 
"@[bench_gen: generates benchmarks for incl2pg@]@.\
@[Usage: bench_gen <subdir> <generator> [generator parameters]@]@.@.\
@[Benchmarks are saved in subdirectories subdir1,subdir2,...@ \
Note that all paths are relative.@ Ex: bench_gen@ mydir [..]@ \
creates@ a@ subdirectory@ mydir@ of@ the@ directory@ of@ benchmarks@ \
($cwd/../benchmarks)@]@.@.\
@[<v 4>@[Available methods : %t@]%t@]@]@.@.@[<v 4>Examples :%t@]@." 
pp_available_methods pp_descr pp_examples		   
; 
exit 0 

let _ = 
  let args = ref [] in
  Arg.parse [] (fun s -> args := s :: !args) "";
  let args = List.rev !args in
  let (subdir,g_name,args) = match args with
  | subdir::g_name::args -> (subdir,g_name,Array.of_list args)
  | _ -> inv_param () in
  let gen = 
    try 
      Some(List.find (fun g->g.g_name=g_name) generators_list) 
    with _ -> inv_param () in
  match gen with | None -> () |Some(gen) ->
    if (Array.length args)<> gen.g_arity 
    then Format.printf "Wrong parameters@."
    else gen.g_function args subdir
	

      
