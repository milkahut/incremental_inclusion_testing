open Benchmarking
open Arg

let _ = Random.self_init ()

let cwd = Sys.getcwd()

(* PARAMETERS *)

(* general *)
let max_i = ref (30) 
let min_i = ref (10) 
let step_i = ref (1)     
let iterations = ref (50) 

let nbproc = ref (16)
let curves_only = ref(false)
let skip_gen = ref(false)
let default_bench_name = "tmp_bench"
let save_as = ref(default_bench_name)

let gtimeout = ref (10)
let mtimeout = ref(60)

let tv_split_name = "tv_split"
let gen_name = ref (tv_split_name)

let use_goal = ref(false)

(* Tabakov Vardi *)
let alphabetsize = ref (2) 
let tr_density = ref (0.8)
let acc_density = ref (0.3)

(* Obfuscation by Splitting *)
let p1 = ref (0.55)
let p2 = ref (0.45)
let split_ratio = ref (0.5)

(* parameters shown on the first page of the latex file *)
type myval = 
  | Float of float ref 
  | Int of int ref
  | String of string ref

let params_shown_in_latex =
  ["-iter", "number of iterations for each i",Int(iterations),"General"; 
   "-gtimeout","timeout for one k",Int(gtimeout),"General";
   "-mtimeout","timeout over all k",Int(mtimeout),"General";
   "-o", "name of the dir", String(save_as), "General";
   "-alph","alphabet size",Int(alphabetsize),"Tabakov-Vardi";
   "-tr_dens","transition density",Float(tr_density),"Tabakov-Vardi";
   "-acc_dens","accepting statess' density",Float(acc_density),"Tabakov-Vardi";
   "-p1","probability p1",Float(p1),"Split-state Obfuscator";
   "-p2","probability p2",Float(p2),"Split-state Obfuscator";
   "-ratio","ratio of splitted states",Float(split_ratio),"Split-state Obfuscator"]

let get_arg_constr = function
  | Float(f) -> Set_float(f)
  | Int(i) -> Set_int(i)
  | String(s) -> Set_string(s)


let str_of_param = function
  | Float(f) -> Format.sprintf "%4.1f" !f
  | Int(i) -> Format.sprintf "%i" !i
  | String(s) -> !s

let help_message_of_param (_,desc,param,section) =
  Format.sprintf "set %s [Sec:%s , Default:%s]" desc section (str_of_param param)

let commands = 
  ["-from", Set_int(min_i), "";
   "-to", Set_int(max_i), "";
   "-step", Set_int(step_i), "";
   "-nbproc",Set_int(nbproc), "set the number of parallel processes";
   "-c", Set(curves_only), "generate curves only";
   "-cc", Set(skip_gen), "skip generation phase";
   "-goal", Set(use_goal), "check inclusion with goal";
  ] @
    let f ((flag,desc,param,section) as x) = 
      (flag,get_arg_constr param,help_message_of_param x) in
    List.map f params_shown_in_latex

let help_message =
  "curves_gen : generates gen from benchmarks\n" 
^ "Usage: curves_gen [options] -o mydir \n" 
^ "Effect: generates benchmarks, stats, and ../benchmarks/mydir/curves/main.tex\n\n"




(* MISC *)

let forall_bench f =
  for i = 0 to (!max_i- !min_i)/ !step_i do
    for it = 1 to !iterations do
      f (!min_i+i* !step_i) it
    done
  done


let command str = 
  Format.printf "%s@." str;
  ignore (Sys.command str)

let nb_running_procs = ref 0

let do_in_parallel l nbsubproc = 
  let nbproc = !nbproc / nbsubproc in
  let spawn f =
    if !nb_running_procs = nbproc 
    then ignore (Unix.wait())
    else nb_running_procs:=!nb_running_procs+1;
    if Unix.fork()=0 then begin
      f ();
      exit 0
    end in
  List.iter spawn l;
  while !nb_running_procs>0 do 
    ignore (Unix.wait()); 
    nb_running_procs:=!nb_running_procs-1
  done

let benchname size it = Format.sprintf "%s/%i-%i" !save_as size it

let fpath size it = 
  Format.sprintf "%s/../benchmarks/%s" cwd (benchname size it)

let fnameA size it = (fpath size it)^ "/A.gff"

let fnameB size it = (fpath size it)^ "/B.gff"

let fname_goal_out size it = (fpath size it)^ "/goal.out"

let fpath_curves () = 
  Format.sprintf "%s/../benchmarks/%s/curves/" cwd !save_as

let fname_dat_for_latex method_name measure_name = 
  Format.sprintf "%s-%s.dat" method_name measure_name

let fname_dat method_name measure_name = 
  Format.sprintf "%s%s" (fpath_curves()) (fname_dat_for_latex method_name measure_name)

let fname_latex () =
  Format.sprintf "%smain.tex" (fpath_curves())

let fname_params () =
  Format.sprintf "../benchmarks/%s/params.txt" !save_as



(* BENCHMARKS GENERATORS *)

let save_params() =
  let outch = open_out (fname_params ()) in
  let fmt = Format.formatter_of_out_channel outch in
  let f (_,descr,param,_) = Format.fprintf fmt "%s@.%s@." 
    descr (str_of_param param) in
  List.iter f params_shown_in_latex;
  close_out outch

let load_params() = 
  let inch = open_in (fname_params ()) in
  let f (_,_,param,_) = 
    ignore (input_line inch);
    match param with
      | Float(f) -> f:= float_of_string (input_line inch)
      | Int(i) -> i:= int_of_string (input_line inch)
      | String(s) -> s:= input_line inch
  in List.iter f params_shown_in_latex;
  close_in inch
  

let tv_and_split_gen size _it =
  let size1 = int_of_float ((1.-. !split_ratio)*. (float_of_int size)) 
  in
  let auta = Nba_examples.random_nba !alphabetsize size1 
    !tr_density !acc_density in
  let autb = Nba.state_splitting auta (size-size1) !p1 !p2 in
  let autc = Nba.state_splitting auta (size-size1) !p1 !p2 in
  (autb,autc)

let split_gen_seed = ref None

let split_gen size _it = 
  let (auta,size2) = match !split_gen_seed with
    | Some(auta,size2) -> auta,size2
    | None -> begin
      let auta = Nba_examples.random_nba !alphabetsize (size -1) !tr_density !acc_density in
      split_gen_seed := Some(auta,(size-1));
      (auta,size-1)  
    end in 
  let autb = Nba.state_splitting auta (size-size2) !p1 !p2 in 
  (auta,autb)

let compl_diff_gen size it = 
  let seed_file = 
    Format.sprintf "../benchmarks/%s/seed%i.gff" !save_as size in  
  if not (Sys.file_exists seed_file) then begin
    Gff_tools.gff_file_of_aut_array (Gff_tools.goal_rand_nba size !alphabetsize !tr_density !acc_density)
      seed_file;
    command (Format.sprintf 
               "./bench_gen \"%s/%i-\" compldiff %s"
               !save_as size seed_file);
    let nb_gen = ref(0) in
    for i=1 to !iterations do 
      if not (Sys.file_exists (Format.sprintf "../benchmarks/%s/%i-%i" 
                             !save_as size i))
      then begin
        if !nb_gen = 0 then nb_gen := i-1;
        let j = !nb_gen - (i mod !nb_gen) in
        command (Format.sprintf "ln -s %s %s" (fpath size j) (fpath size i))
      end
    done
  end;
  ((Nba.nba_of_gff_file (fnameA size it)),
    (Nba.nba_of_gff_file (fnameB size it)))
  
let bench_gen_list =
  [tv_split_name,(tv_and_split_gen,"Tabakov-Vardi and split obfuscation");
   "split_gen",(split_gen,"Split Obfuscation over a constant NBA generated with Tabakov Vardi");
   "compl_diff",(compl_diff_gen,"compldiff over a rand NBA of size i") 
  ]

let bench_gen_list_str = 
  let l = List.map fst bench_gen_list in
  let default = List.hd l in
  let others = List.map (fun str ->("|"^str)) (List.tl l) in
  List.fold_left (^) default others

let get_gen () = 
  fst (List.assoc !gen_name bench_gen_list)

let get_gen_desc () = 
  snd (List.assoc !gen_name bench_gen_list)

let commands = 
commands @ ["-g",Set_string(gen_name),
            ("[" ^ bench_gen_list_str ^ "] : selects a generator")
           ]


(* RUNNING EXPERIMENTS *)

let treat_bench_with_goal size it =
  let f_auta,f_autb,f_goal = (fnameA size it),(fnameB size it),(fname_goal_out size it) in
  let cmdline = 
  Format.sprintf "time %s containment %s %s >%s 2>%s" 
   (Gff_tools.goal_absolute()) f_auta f_autb f_goal f_goal in
  command cmdline


let treat_bench size it =
  if !use_goal then treat_bench_with_goal size it 
  else command (Format.sprintf "./incl2pg_bench -nbproc 3 -gtimeout %i -mtimeout %i %s" !gtimeout !mtimeout (benchname size it))


let gen_bench generator size it = 
  let (auta,autb) = generator size it in
  command (Format.sprintf "mkdir %s" (fpath size it));
  Nba.gff_file_of_nba auta (fnameA size it);
  Nba.gff_file_of_nba autb (fnameB size it)




let run_experiments () = 
  command (Format.sprintf "mkdir ../benchmarks/%s" !save_as);
  let todo = ref [] in
  let generator = get_gen () in
  let prepare_it size it = 
    if not !skip_gen then gen_bench generator size it;
    todo:=(fun ()->treat_bench size it)::!todo 
  in forall_bench prepare_it;
  if not !skip_gen then save_params();
  do_in_parallel !todo 3




(* DAT FILES GENERATION *)

type bench_stat = {
  s_pebble : game_stat;
  s_static : game_stat;
  s_dynamic : game_stat;
  s_goal : game_stat;
}

let default_bench_stat = {
  s_pebble = default_gamestat;
  s_static = default_gamestat;
  s_dynamic = default_gamestat;
  s_goal  = default_gamestat    
}

let stats = ref ([||]) 

let sel_static s = s.s_static
let sel_dynamic s = s.s_dynamic
let sel_pebble s = s.s_pebble
let sel_goal s = s.s_goal

let sel_list = 
  ["static",sel_static;
   "dynamic",sel_dynamic;
   "pebble",sel_pebble;
   "goal",sel_goal]

let comparison_list =
  ["pebble",sel_pebble,"red","*","1pt",".stat";
   "static",sel_static,"blue","triangle*","1.5pt",".stat";
   "dynamic",sel_dynamic,"green!50!black","square*","1pt",".stat";
   "goal",sel_goal,"black","pentagon","1pt",".out"]

let alloc_stats () =
  stats :=
  Array.init (!max_i- !min_i+1) (fun _ ->Array.make !iterations default_bench_stat) 

let collect_stats size it = 
  !stats.(size- !min_i).(it-1)<-{
    s_pebble = get_stat (benchname size it) "pebble";
    s_static = get_stat (benchname size it) "static";
    s_dynamic = get_stat (benchname size it) "dynamic";
    s_goal = default_gamestat (* TODO: parse goal stats *) 
  }

let out_dat dat fname =
  let ch = open_out fname in
  let fmt = Format.formatter_of_out_channel ch in
  let f (x,y) = match y with
    | None -> ()
    | Some(y) -> Format.fprintf fmt "%f             %f@." x y 
  in Array.iter f dat;
  close_out ch
    
let average selector measure condition =
  let av stats_of_one_size =
    let nb = ref 0 in
    let sum = ref 0. in
    let f stat =
      if (condition stat) then begin
        nb:=!nb+1;
        sum :=!sum +. measure stat
      end in
    Array.iter f stats_of_one_size;
    if !nb=0 then None
    else Some ((!sum) /. (float_of_int !nb)) in
  let f i stats_of_one_size =
    ((float_of_int (!min_i + i)),av (Array.map (snd selector) stats_of_one_size)) in
  Array.mapi f !stats 

let perc_smallest selector measure condition =
  let smaller x y = ((y -. x) /. x) > 0.1 in
  let is_best selector bench_stat =
    let sel = snd selector in
    if condition (sel bench_stat) then begin
      let selected = measure (sel bench_stat) in
      let f (_,sel) = 
        if condition (sel bench_stat) 
        then measure (sel bench_stat) else 1000. *. selected in
      let others = 
        List.map f (List.filter (fun sel->((fst sel)<>(fst selector))) sel_list) in
      List.for_all (smaller selected) others 
    end else false  in
  let has_a_winner bench_stat = 
    List.exists (fun sel->is_best sel bench_stat) sel_list in
  let f i stats_of_one_size = 
    let nb_best = List.length (List.filter (is_best selector) (Array.to_list stats_of_one_size)) in
    let nb_all = List.length (List.filter has_a_winner (Array.to_list stats_of_one_size)) in
    (float_of_int (!min_i + i),
     if nb_all = 0 then None 
     else Some (((float_of_int nb_best) /. (float_of_int nb_all)) *. 100. )) 
  in Array.mapi f !stats


let maximal selector measure condition :(float * float option) array=
  let maximum x l = List.fold_left max x l in
  let f i stats_of_one_size =
    let l = 
      (List.filter condition 
         (Array.to_list (Array.map (snd selector) stats_of_one_size))) 
    in 
    (float_of_int (!min_i + i)),
    if List.length l = 0 then None 
    else Some (maximum 0. (List.map measure l))
  in Array.mapi f !stats

let cond_none stat = stat <> default_gamestat

let cond_success stat = stat.success

let meas_k stat = float_of_int (stat.n_iteration)

let meas_cstime stat = max 0. (stat.ct +. stat.st)

let meas_tttime stat = max 0. stat.mt

let meas_nodes stat = (float_of_int stat.n_nodes) /. 1000.

let meas_tr stat = (float_of_int stat.n_tr) /. 20000.

let meas_success stat = if stat.success then 100. else 0.

let graph_list = 
  ["k","k (average smallest ensuring success)",average,cond_success,meas_k,1.,3.;
   "k_max","max. k (maximal smallest ensuring success)",maximal,cond_success,meas_k,1.,9.;
   "t","time needed in case of success (in sec)",average,cond_success,meas_k,0.,8.; 
   "success","success rate (\\%)",average,cond_none,meas_success,0.,100.;
   "size","number of nodes $\\times 10^{-3}$ in case of success",average,cond_success,meas_nodes,0.,100.;
   "size_tr","number of transitions $\\times 0.5 \\cdot 10^{-4}$ in case of success",average,cond_success,meas_tr,0.,100.;
   "fastest","race winning rate (\\%)",perc_smallest,cond_success,meas_cstime,0.,100.;
   "fastest2","race2 winning rate (\\%)",perc_smallest,cond_success,meas_tttime,0.,100.;
   "maxtime","max. time needed for success (in sec)",maximal,cond_success,meas_cstime,0.,60.;
]


let gen_dats () =
  command (Format.sprintf "mkdir ../benchmarks/%s/curves" !save_as);
  alloc_stats();
  forall_bench collect_stats;
  let f (meas_fname,_,f,cond,meas,_,_) sel =
    out_dat (f sel meas cond) 
      (fname_dat (fst sel) meas_fname)
  in List.iter (fun x->List.iter (f x) sel_list) graph_list


let color = [|"";"red";"blue";"green!50!black"|]
  
let gen_tikzpicture fmt (meas_fname,meas_desc,_,_,_,ymin,ymax) =
  let xstep =  
    List.find (fun step-> ((!max_i- !min_i) / step <8))
      [2;5;10;25;50;100;500] in
  let ystep = (ymax -. ymin) *. 0.25 in
  Format.fprintf fmt "\\newpage@.@.\\begin{tikzpicture}[x=%4.1fcm,y=%4.1fcm]"
    (15. /. (float_of_int (!max_i - !min_i))) (8. /. (ymax -. ymin));
  Format.fprintf fmt "\\def\\xmin{%i}@.\\def\\xmax{%4.1f}@."
    !min_i ((float_of_int !max_i)+. 0.04 *. (float_of_int xstep));
  Format.fprintf fmt "\\def\\ymin{%4.1f}@.\\def\\ymax{%4.1f}@.@."
    ymin (ymax+. (ystep *. 0.75));
  Format.fprintf fmt 
    "\\draw[style=help lines, ystep=%4.1f, xstep=%i] (\\xmin,\\ymin) grid"
    (ystep *. 0.5) (xstep / 2);
  Format.fprintf fmt "%s@.@.%s@."
    "(\\xmax,\\ymax);" 
    "\\draw[->] (\\xmin,\\ymin) -- (\\xmax,\\ymin) node[right] {size};";
  Format.fprintf fmt
    "\\draw[->] (\\xmin,\\ymin) -- (\\xmin,\\ymax) node[above] {%s};@.@."
    meas_desc;
  Format.fprintf fmt "\\foreach \\x in {%i,%i,...,%i} " 
    !min_i (!min_i+ xstep) !max_i;
  Format.fprintf fmt "%s@."
    "\\node at (\\x, \\ymin) [below] {\\x};";
  Format.fprintf fmt 
    "\\foreach \\y in {%4.1f,%4.1f,...,%4.1f} "
    ymin (ymin+. ystep) ymax;
  Format.fprintf fmt 
    "\\node at (\\xmin,\\y) [left] {\\y};@.@.";
  Format.fprintf fmt "\\node (0) at (\\xmax,\\ymax) {};";
  let i =ref 0 in
  let f (method_name,_) = 
    i:= !i+1;
    Format.fprintf fmt
      "\\draw[color=%s] plot[smooth,mark=*,mark size=1pt] file {%s} node [right] {};@."
      color.(!i) (fname_dat_for_latex method_name meas_fname);
    Format.fprintf fmt
      "\\node[color=%s,below of=%i] (%i) {%s};@.@."
      color.(!i) (!i-1) !i method_name
  in List.iter f sel_list;
  Format.fprintf fmt "\\end{tikzpicture}@."
    
    
let gen_latex () =
  let ch = open_out (fname_latex ()) in
  let fmt = Format.formatter_of_out_channel ch in
  Format.fprintf fmt "%s@.%s@.%s@.%s@.%s.@.@."
    "\\documentclass[landscape]{article}"
    "\\usepackage{tikz}"
    "\\usepackage{fullpage}"
    "\\begin{document}"
    "\\pagestyle{empty}";
  Format.fprintf fmt "\\section*{Benchmark Generation}@.%s@." 
    (get_gen_desc());
  let last_sec = ref("") in
  let f (_,desc,param,section) = 
    if !last_sec <> section then begin
      last_sec:=section;
      Format.fprintf fmt "\\section*{%s}@." section
    end else
      Format.fprintf fmt "\\\\@." ;
    Format.fprintf fmt "%s: \\emph{%s}" desc (str_of_param param)
  in List.iter f params_shown_in_latex;
  List.iter (gen_tikzpicture fmt) graph_list;
  Format.fprintf fmt "@.@.\\end{document}@.";
  close_out ch
    

let show_output () =
  command 
    (Format.sprintf 
       "cd ../benchmarks/%s/curves;pdflatex main;open main.pdf;cd ../../../tool/"
    !save_as)

(* MAIN *)

let _ = 
  parse commands (fun _ -> ()) help_message;
  if not !curves_only then run_experiments ();
  load_params();
  gen_dats();
  gen_latex();
  show_output();
