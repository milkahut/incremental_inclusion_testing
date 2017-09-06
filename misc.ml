open Unix
(* set to true in order to obtain some output which may be useful for debugging *)
let debug = ref false

let message s = if !debug then 
                  begin
                    print_string (s ());
                    print_newline ()
                  end


(* type for incl_to_pg representation of automata *)
type aut_array = (int list array * bool) array

(* search exec command for goal*)
let goal_abs = ref None

let goal_absolute () =
  match !goal_abs with
    | None ->
        let goalpath = 
          try
            let ch = (open_in "goalpath") in
            let str = input_line ch in
            close_in ch;
            str 
          with _ -> Format.fprintf Format.err_formatter 
           "could not find goal path in \"goalpath\" file"; exit 0
        in 
        let res = goalpath^"goal" in
        goal_abs := Some(res);
        res
    | Some(res) -> res


exception Goal_failure of string

let goal_error msg = raise (Goal_failure msg)

(*
let language_inclusion auta autb = 
  try
    let cwd = Sys.getcwd () in
    let (f_auta,f_autb) = (cwd ^ "/tmp/A.gff"), (cwd ^ "/tmp/B.gff") in
    gff_file_of_aut_array auta f_auta; 
    gff_file_of_aut_array autb f_autb;
    let cmdline = Format.sprintf "%s containment %s %s" (goal_absolute()) f_auta f_autb in
    let ch = Unix.open_process_in cmdline in
    let goalres = (input_line ch) in
    close_in ch;
    let index = String.index goalres '(' in
    if goalres.[index+1]='t' then true
    else if goalres.[index+1]='f' then false
    else goal_error ("could not parse bool in goal result == " ^ goalres) 
  with 
  | Goal_failure str -> goal_error str
  | Invalid_argument str -> goal_error ("Invalid argument (" ^ str ^ ")")
  | Unix.Unix_error(err,fname,farg) ->goal_error (Format.sprintf "Unix call failure with %s(%s). %s" fname farg (Unix.error_message err))
*)


let expand_dot_dot_notation str_list =
  let num c = 
    if c='.' then 10
    else let res = ((int_of_char c)-(int_of_char '0')) in
    if (res>=0) && (res<=10) then res else -1 
  in
  let f str =
    try 
      let i = ref ((String.length str) -1) in
      while (num (str.[!i]))<>10 do i:=!i-1 done;
      let index1 = !i in
      while (num (str.[!i]))>=0 do i:=!i-1 done;
      let index2 = !i +1 in
      let first = 
       int_of_string (String.sub str index2 (index1-index2-1)) in
      let last = 
       int_of_string (String.sub str (index1+1) ((String.length str)-index1-1)) in
      let pref = String.sub str 0 index2 in
      let rec list i = 
       if i<=last then (pref^(string_of_int i))::list (i+1)
       else [] in
      list first
    with _ -> [str] in
  List.flatten (List.map f str_list)

(*
let goal_rand_nba size alpha_size tr_density acc_density =
  let nb_prop_var = 
    let rec nb_digits x = if x>1 then 1+ nb_digits (x/2) else 1
    in nb_digits (alpha_size-1) in
  let cmdline = 
    Format.sprintf 
      "%s generate -t fsa -a nbw -s %i -n %i -pt %4.1f -pa %4.1f"
      (goal_absolute()) size nb_prop_var tr_density acc_density      
  in Format.printf"%s\n" cmdline;
  let inch = Unix.open_process_in cmdline in
  let res = aut_array_of_in_channel inch in
  ignore (Unix.close_process_in inch);
  res
*)

let pp_time fmt t =
  if t>1. then begin
    let t = gmtime t in
    if t.tm_mday>2 then Format.fprintf fmt "%i days " (t.tm_mday-1)
    else if t.tm_mday=2 then Format.fprintf fmt "1 day ";
    if t.tm_hour>0 then Format.fprintf fmt "%i hour " t.tm_hour;
    if t.tm_min>0 then Format.fprintf fmt "%i min " t.tm_min;
    if t.tm_sec>0 then Format.fprintf fmt "%i sec" t.tm_sec
  end else Format.fprintf fmt "%4.2f sec" t


(* parallelisation *)
let nbproc=ref(1)

let nb_running_procs = ref 0

let iter_in_parallel f l = 
  let spawn arg =
    if !nb_running_procs = !nbproc 
    then ignore (wait())
    else nb_running_procs:=!nb_running_procs+1;
    if fork()=0 then begin
      f arg;
      exit 0
    end in
  List.iter spawn l;
  while !nb_running_procs>0 do 
    ignore (wait()); 
    nb_running_procs:=!nb_running_procs-1
  done


let do_in_parallel l = 
  let spawn f =
    if !nb_running_procs = !nbproc 
    then ignore (wait())
    else nb_running_procs:=!nb_running_procs+1;
    if fork()=0 then begin
      f ();
      exit 0
    end in
  List.iter spawn l;
  while !nb_running_procs>0 do 
    ignore (wait()); 
    nb_running_procs:=!nb_running_procs-1
  done


(* timer functions *)
exception Time_out

let sigalrm = Sys.sigvtalrm

let timertype = ITIMER_VIRTUAL

let set_timer t = 
  ignore (setitimer timertype {it_interval=0.0;it_value=t})

let watch_stop_timer () = 
  let {it_value=t} = setitimer timertype {it_interval=0.0;it_value=0.0}
  in t      
 
let with_time_out timeout f =
  let oldsig = 
    Sys.signal sigalrm (Sys.Signal_handle (fun _->raise Time_out)) in
  let kill_timer () =
    Sys.set_signal sigalrm oldsig in
  try
    set_timer timeout;
    let res = f() in
    let t = watch_stop_timer() in
    kill_timer();
    (res,timeout-.t)
  with 
    Time_out -> kill_timer();raise Time_out


let shell_with_time_out timeout cmd = 
  let cmd2 = 
    Format.sprintf "perl -e 'alarm shift @@ARGV; exec @@ARGV' %f %s"
      timeout cmd in
  let ch = open_process_in cmd2 in
  let res = ref [] in
  let rec read () =
    res := (input_line ch):: !res;
    read() in
  try read () with End_of_file -> !res

(* dat files *)
type dat = (float * float) array

let read_dat fname = 
  let l = ref ([]) in
  let add x y = l:= ((x,y):: !l) in
  let cut_at_space str = 
    let i = String.index str ' ' in
    (String.sub str 0 i),(String.sub str i (String.length str - i)) in
  let rec parse ch = 
    try
      let str1,str2 = cut_at_space (input_line ch) in
      add (float_of_string str1) (float_of_string str2);
      parse ch 
    with
    | End_of_file -> () in
  let ch = open_in fname in
  parse ch;
  close_in ch;
  Array.of_list !l
  
let write_dat dat fname = 
  let ch = open_out fname in
  let fmt = Format.formatter_of_out_channel ch in
  let f (x,y) = Format.fprintf fmt "%f   %f@." x y in
  Array.iter f dat;
  close_out ch



(* latex functions *)
let begin_document fmt = 
  Format.fprintf fmt "\
\\documentclass[landscape]{article}@.\
\\usepackage{tikz}@.\
\\usepackage{fullpage}@.\
\\begin{document}@.\
\\pagestyle{empty}@."

let end_document fmt =
  Format.fprintf fmt "\\end{document}@."
let pp_table fmt table = 
  let f line =
    Array.iter (Format.fprintf fmt "%s & ") line;
    Format.fprintf fmt "\\\\ \\hline@." in
  Format.fprintf fmt "\\begin{tabular}{|c|";
  Array.iter (fun _->Format.fprintf fmt "c|") table.(0);
  Format.fprintf fmt "}@.\\hline@.";
  Array.iter f table;
  Format.fprintf fmt "\\end{tabular}@."

type color = string
let blue = "blue"
let red = "red"
let green = "green!50!black"
let brown = "brown"
let pink = "pink"

type mark = string
let dot = "*,mark size=1pt"
let triangle = "triangle*,mark size=1.5pt"
let square = "square*,mark size=1pt"

type curve = {
    cu_name: string; (** name of the curve *)
    cu_fname: string; (** name of the file that should be plot *)
    cu_color: color; (** color of the curve *)
    cu_mark: mark; (** mark for a point *)
}

let pp_curves fmt xlabel ylabel curves = 
  let all_dat = List.map (fun cu->read_dat cu.cu_fname) curves in
  let actualize_bounds (xmin,xmax,ymin,ymax) (x,y) = 
    (min xmin x),(max xmax x),(min ymin y),(max ymax y) in
  let actualize_bounds2 bounds dat= 
    Array.fold_left actualize_bounds bounds dat in
  let xmin,xmax,ymin,ymax = 
    List.fold_left actualize_bounds2 (0.,0.,0.,0.) all_dat in
  let xstep =  
    List.find (fun step-> ((xmax-. xmin) /. step <8.))
      [2.;5.;10.;25.;50.;100.;500.] in
  let ystep = (ymax -. ymin) *. 0.25 in
  Format.fprintf fmt "\\begin{tikzpicture}[x=%4.1fcm,y=%4.1fcm]"
    (15. /. (xmax -. xmin)) (8. /. (ymax -. ymin));
  Format.fprintf fmt "\\def\\xmin{%4.1f}@.\\def\\xmax{%4.1f}@."
    xmin (xmax +. 0.04 *. xstep);
  Format.fprintf fmt "\\def\\ymin{%4.1f}@.\\def\\ymax{%4.1f}@.@."
    ymin (ymax+. (ystep *. 0.75));
  Format.fprintf fmt 
    "\\draw[style=help lines, ystep=%4.1f, xstep=%4.1f] (\\xmin,\\ymin) grid (\\xmax,\\ymax);@."
    (ystep *. 0.5) (xstep /. 2.);
  Format.fprintf fmt "\\draw[->] (\\xmin,\\ymin) -- (\\xmax,\\ymin) node[right] {%s};@."
     xlabel;
  Format.fprintf fmt
    "\\draw[->] (\\xmin,\\ymin) -- (\\xmin,\\ymax) node[above] {%s};@.@."
    ylabel;
  Format.fprintf fmt "\\foreach \\x in {%4.1f,%4.1f,...,%4.1f} \\node at (\\x, \\ymin) [below] {\\x};@." 
    xmin (xmin +. xstep) xmax;  
  Format.fprintf fmt "\\foreach \\y in {%4.1f,%4.1f,...,%4.1f} \\node at (\\xmin,\\y) [left] {\\y};@.@."
    ymin (ymin+. ystep) ymax;
  Format.fprintf fmt "\\node (0) at (\\xmax,\\ymax) {};@.";
  Format.fprintf fmt "\\node (1) [left of=0]{};@.";  
  let i= ref (1) in
  let f curve = 
    i:= !i+1;
    Format.fprintf fmt
      "\\draw[color=%s] plot[smooth,mark=%s] file {%s} node {};@."
      curve.cu_color curve.cu_mark curve.cu_fname;
    Format.fprintf fmt
      "\\node[color=%s,below of=%i] (%i) {%s};@.@."
      curve.cu_color (!i-1) !i curve.cu_name
  in List.iter f curves;
  Format.fprintf fmt "\\end{tikzpicture}@."


(* subfolders and files manipulation *)

let rec get_all_files dir =
  let all = List.map (fun f -> dir ^ "/" ^ f) (Array.to_list (Sys.readdir dir)) in
  let (dirs,files) = List.partition (fun file -> Sys.is_directory file) all in 
    (List.flatten (List.map (fun d -> get_all_files d) dirs)) @ (files)
let dir filename = 
  let slash_pos = String.rindex filename '/' in
    String.sub filename 0 slash_pos
let is_A_gff filename = 
  let n = String.length filename in 
    String.sub filename (n-5) 5 = "A.gff" 
let exists_B_gff dir = 
  let l = get_all_files dir in
    List.exists (fun x -> x = dir^"/B.gff") l
let get_all_bench directory =
  let l = get_all_files directory in
  let gff_files = List.filter (fun x -> is_A_gff x && exists_B_gff (dir x)) l in
    List.map dir gff_files
let get_subdir directory = 
  let all_files_and_subdir = Array.to_list (Array.map (fun x -> directory^"/"^x) (Sys.readdir directory)) in
    List.filter Sys.is_directory all_files_and_subdir
let get_foldername path = 
  let i = try String.rindex path '/' with Not_found -> (-1) in
    String.sub path (i+1) ((String.length path)-(i+1))
