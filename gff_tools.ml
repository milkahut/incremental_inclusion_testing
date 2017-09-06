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


(* types for the gff representation of automata *)
type state = int
type letter = string
type transition = state * letter * state

module StateSet = 
  Set.Make(struct type t=state let compare=compare end)
module TransitionSet = 
  Set.Make(struct type t=transition let compare=compare end)
type alphabet = (letter,int) Hashtbl.t


(* type of infos collected while parsing a gff file *)
type gff_info = {
    mutable state_set : StateSet.t;
    mutable acc_state_set : StateSet.t;
    mutable transition_set : TransitionSet.t;
    mutable alphabet : alphabet;
  }

let new_gff_info () = {
    state_set = StateSet.empty;
    acc_state_set = StateSet.empty;
    transition_set = TransitionSet.empty;
    alphabet = Hashtbl.create 20;
  }

let add_state gff_info s =
  gff_info.state_set<-StateSet.add s gff_info.state_set 

let add_acc_state gff_info s =
  gff_info.acc_state_set<-StateSet.add s gff_info.acc_state_set 

let add_tr gff_info tr =
  gff_info.transition_set<-TransitionSet.add tr gff_info.transition_set 

let add_ltr gff_info ltr = 
  let alph = gff_info.alphabet in 
  if not (Hashtbl.mem alph ltr)
  then Hashtbl.add alph ltr (Hashtbl.length alph)


(* parsing functions *)
let parse_error msg = Format.printf "WARNING: %s.@." msg

let parse_state_node gff_info = function
  | Xml.Element(tagname,attributes,_) when tagname="State" ->
      begin
	try
	  let s = int_of_string (List.assoc "sid" attributes) in
	  add_state gff_info s
	with
	| Not_found | Failure _ -> 
	    parse_error "State node ill-formed (ignored)"
      end
  | _ -> parse_error "State node expected"


let parse_stateset gff_info = function
  |Xml.Element(tagname,_,states) when tagname="StateSet" ->
      List.iter (parse_state_node gff_info) states
  | _ -> ()

let rec xml_mem tag = function
  | [] -> raise Not_found
  | Xml.Element(tagname,_,[Xml.PCData(res)])::_ when tagname=tag -> res
  | _::tl -> xml_mem tag tl

let parse_transition gff_info = function
  | Xml.Element(tagname,_,attributes) when tagname ="Transition"->
      begin
	try
	  let src = int_of_string (xml_mem "From" attributes) in
	  let dst = int_of_string (xml_mem "To" attributes) in
	  let ltr = xml_mem "Label" attributes in
	  add_tr gff_info (src,ltr,dst);
	  add_ltr gff_info ltr
	with
	| Not_found | Failure _ -> parse_error "Transition node ill-formed (ignored)"
      end
  | _ -> parse_error "Transition node expected"

let parse_transitionset gff_info = function
  | Xml.Element(tagname,_,trlist) when tagname="TransitionSet" ->
      List.iter (parse_transition gff_info) trlist
  | _ -> ()

let parse_accstate gff_info = function
  | Xml.Element(tagname,_,[Xml.PCData(s)]) when tagname = "StateID" ->
      begin
	try add_acc_state gff_info (int_of_string s)
	with Failure _ -> parse_error "StateID node ill-formed (ignored)"
      end
  | _ -> parse_error "StateId  node expected"

let parse_accstateset gff_info = function
  | Xml.Element(tagname,_,st_list) when tagname="Acc" ->
      List.iter (parse_accstate gff_info) st_list
  | _ -> ()


(* main functions *)

let gff_info_of_xml xml_root =
  let gff_info = new_gff_info () in
  let level1  = match xml_root with
  | Xml.Element(_,_,level1) -> level1
  | Xml.PCData(_) -> invalid_arg "empty xml" in
  List.iter (parse_stateset gff_info) level1;
  List.iter (parse_transitionset gff_info) level1;
  List.iter (parse_accstateset gff_info) level1;
  gff_info

let automaton_array_of_gff_info gff_info = 
  let nl = Hashtbl.length gff_info.alphabet in
  let ns = 1 + StateSet.max_elt gff_info.state_set in
  let init i = 
    (Array.make nl []),(StateSet.mem i gff_info.acc_state_set) in
  let a = Array.init ns init in
  let addtr (src,ltr,dst) = 
    let ltr = Hashtbl.find gff_info.alphabet ltr in
    (fst a.(src)).(ltr)<-(dst::(fst a.(src)).(ltr)) in
  TransitionSet.iter addtr gff_info.transition_set;
  a


let aut_array_of_gff_file filename =
  let xml_root = Xml.parse_file filename in
  let gff_info = gff_info_of_xml xml_root in
  automaton_array_of_gff_info gff_info 

let aut_array_of_in_channel inch = 
  let xml_root = Xml.parse_in inch in
  let gff_info = gff_info_of_xml xml_root in
  automaton_array_of_gff_info gff_info 


let gff_file_of_aut_array a filename = 
  let pp_attr fmt (x,y) = 
    Format.fprintf fmt " %s=\"%s\"" x y in
  let pp_node fmt tag attrs pp_children =
    Format.pp_open_vbox fmt 3;
    Format.fprintf fmt "<%s%t>@,%t"
      tag (fun fmt->List.iter (pp_attr fmt) attrs)
      pp_children;
    Format.pp_close_box fmt ();
    Format.fprintf fmt "</%s>@," tag in
  let pp_stateset fmt =
    pp_node fmt "StateSet" [] (fun fmt->
    for i=0 to Array.length a-1 do
      pp_node fmt "State" ["sid",(string_of_int i)] (fun _fmt->())
    done) in
  let pp_initialstateset fmt =
    pp_node fmt "InitialStateSet" [] (fun fmt->
      Format.fprintf fmt "<StateID>0</StateID>") in
  let pp_alphabet fmt = 
    pp_node fmt "Alphabet" ["type","Propositional"] (fun fmt->
      for i=0 to Array.length (fst a.(0))-1 do	
	pp_node fmt "Proposition" [] (fun fmt->
	  Format.fprintf fmt "a%i@," i)
      done) in
  let pp_transitionset fmt =
    pp_node fmt "TransitionSet" [] (fun fmt->
      let tid = ref 0 in
      let pp src ltr dst =
	tid:=!tid+1;
	pp_node fmt "Transition" ["tid",string_of_int !tid] (fun fmt->
	  Format.fprintf fmt 
	    "<From>%i</From><To>%i</To><Label>a%i</Label>@,"
	    src dst ltr) in
      for src=0 to Array.length a-1 do
	for ltr=0 to Array.length (fst a.(0))-1 do 
	  List.iter (pp src ltr) ((fst a.(src)).(ltr))
	done
      done) in
  let pp_acc fmt =
    let pp fmt st = Format.fprintf fmt "<StateID>%i</StateID>" st in
    pp_node fmt "Acc" ["type","Buchi"] (fun fmt ->
      for i=0 to Array.length a-1 do
	if snd a.(i) then pp fmt i
      done) in
  let ch = open_out filename in
  let fmt = Format.formatter_of_out_channel ch in
  Format.fprintf fmt 
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>@.";
  pp_node fmt 
    "Structure" ["label-on","Transition";"type","FiniteStateAutomaton"]
    (fun fmt ->
      pp_alphabet fmt;
      pp_stateset fmt;
      pp_initialstateset fmt;
      pp_transitionset fmt;
      pp_acc fmt
    );
  close_out ch

exception Goal_failure of string

let goal_error msg = raise (Goal_failure msg)

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
