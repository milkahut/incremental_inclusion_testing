type state = int
type symbol = int
type nba = {
    nba_array: (state list array * bool) array;
    nba_letter_int_list: (string *int) list;
  }

let make_state i = i

let get_nba aut = aut

let empty_nba = {nba_array = [||]; nba_letter_int_list = []}

module GuidedChoicesSet = Set.Make(struct 
  type t = state * bool
  let compare = compare
end);;
module FreeChoicesSet = Set.Make(struct
  type t = state * (symbol list) * bool
  let compare = compare
end);;

let is_final a q = snd a.nba_array.(q)

let successors aut q = List.concat (Array.to_list (Array.mapi (fun i -> fun qs -> List.map (fun q -> (q,i)) qs) (fst aut.nba_array.(q))))

let guided_choices aut q w =
  let rec succs qs = function []      -> qs  
                            | (i::is) -> succs (GuidedChoicesSet.fold (fun (q,b) -> fun s -> 
                                                                   List.fold_right 
                                                                      GuidedChoicesSet.add 
                                                                      (List.map (fun q' -> (q',b || is_final aut q'))
                                                                                (fst aut.nba_array.(q)).(i))
                                                                      s) 
                                                                qs 
                                                                GuidedChoicesSet.empty) 
                                               is
  in
  GuidedChoicesSet.elements (succs (GuidedChoicesSet.singleton (q,is_final aut q)) w)

let free_choices aut q k =
  let rec succs qs = function 0 -> qs
                            | i -> succs (FreeChoicesSet.fold 
                                            (fun (q,w,b) -> fun s -> List.fold_right
                                                                       (fun (q',a) -> fun s' -> FreeChoicesSet.add 
                                                                                                  (q',List.append w [a],b || is_final aut q')
                                                                                                  s')
                                                                       (successors aut q)
                                                                       s)
                                            qs
                                            FreeChoicesSet.empty)
                                         (i-1)
  in
  FreeChoicesSet.elements (succs (FreeChoicesSet.singleton (q,[],is_final aut q)) k)

let initial _ = 0

let alphabet_size aut = Array.length (fst aut.nba_array.(initial aut))

let size nba = Array.length nba.nba_array

let show_state = string_of_int

let show_symbol = string_of_int

let make_nba aut =
  let letter_list size = Array.to_list (Array.init size (fun i -> (string_of_int i, i))) in
    {nba_array = aut; nba_letter_int_list = letter_list (Array.length (fst aut.(initial aut)))}

let state_splitting aut n p1 p2 =
  let l = size aut in
  let a = Array.make (l+n) ([||],false) in
  Array.blit aut.nba_array 0 a 0 l;
  for i=0 to (n-1) do
    let q = 1 + (Random.int (l+i-1)) in
    let q' = l+i in
    let (trans,acc) = a.(q) in
    let k = Array.length trans in
    let trans1 = Array.make k [] in
    let trans2 = Array.make k [] in
    for j=0 to k-1 do
      List.iter (fun p -> let r = Random.float 1.0 in
                          if r < p1 then trans1.(j) <- p :: trans1.(j)
                          else if r >= p2 then trans2.(j) <- p :: trans2.(j)) 
                trans.(j)
    done;
    a.(q) <- (trans1,acc);
    a.(q') <- (trans2,acc);
    for j=0 to q' do
      a.(j) <- (Array.map (fun succs -> if List.mem q succs then q' :: succs else succs) (fst a.(j)), snd a.(j))
    done
  done;
  make_nba a


let adjust_array a l = 
  let copy_a = Array.copy a in
  let set a1 a2 (i,j) = a1.(i) <- a2.(j) in
    List.iter (set copy_a a) l;
    copy_a

let adjust_aut aut l = Array.map (fun x -> (adjust_array (fst x) l, snd x)) aut

let conversion_list l1 l2 = 
  let f a = List.find (fun x -> fst x = a) l1 in
  List.map (fun y -> snd (f (fst y)), snd y ) l2

let normalize_pair (nba1,nba2) = 
  nba1, 
  {nba_array = adjust_aut nba2.nba_array (conversion_list nba1.nba_letter_int_list nba2.nba_letter_int_list); 
   nba_letter_int_list = nba1.nba_letter_int_list}
  

let universal_nba l = make_nba [|(Array.make l [0], true)|]

let fooling_nbas n = (make_nba ( Array.init (n+4)
                                   (fun q -> if q <= n       then ([| [q+1]; [];    []    |], false) 
                                             else if q = n+1 then ([| [];    [n+2]; [n+3] |], false)
                                             else                 ([| [q];   [];    []    |], true))),
                      make_nba ( Array.init (n+5)
                                   (fun q -> if q < n        then ([| [q+1];      [];    []    |], false)
                                             else if q = n   then ([| [n+1; n+2]; [];    []    |], false)
                                             else if q = n+1 then ([| [];         [n+3]; []    |], false) 
                                             else if q = n+2 then ([| [];         [];    [n+4] |], false)
                                             else                 ([| [q];        [];    []    |], true)) )) 

let tough_nbas _ = 
  let auta = [| ([| [0;1]; []; [] |],true); 
                ([| []; [2]; [3] |],false); 
                ([| [2]; []; [] |],true); 
                ([| [3]; []; [] |],true) |] 
  in
  let autb = [| ([| [0;1;2]; []; [] |],true); 
                ([| []; [3]; [] |],false); 
                ([| []; []; [4] |],false); 
                ([| [3]; []; [] |],true); 
                ([| [4]; []; [] |],true) |] 
  in
  (make_nba auta, make_nba autb)


let random_nba alphabetsize size tr_density acc_density =
  let aut = Array.init size (fun q -> (Array.make alphabetsize [],false)) in

  for i=0 to size-1 do
    let f = Random.float 1.0 in
    if f <= acc_density then aut.(i) <- (fst aut.(i), true)
  done;

  for a = 0 to alphabetsize-1 do
    for i = 0 to size-1 do
      for j = 0 to size-1 do 
        let f = Random.float 1.0 in
        if f <= tr_density then
          begin
            let (tr,_) = aut.(i) in
            tr.(a) <- j :: tr.(a)
          end
      done
    done
  done;

  make_nba aut

let nba_of_ba_file filename = 
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let a,b = Ba_parser.main Ba_lexer.token lexbuf in
    {nba_array = a; nba_letter_int_list = b}

let break_aut_array aut =
  let trans_list = ref [] in
  let acc_list = ref [] in
  let f s1 (array,bool) = 
    let g letter l = List.iter (fun s2 -> trans_list := (letter,(s1,s2))::!trans_list ) l in
      Array.iteri g array; 
      if bool then acc_list := s1::!acc_list
    in
  Array.iteri f aut;  
  !trans_list, !acc_list 
   
let ba_file_of_nba aut filename =
  let ch = open_out filename in
  let l1,l2 = break_aut_array aut.nba_array in
  let print_trans (a,(s1,s2)) = 
    List.fold_left (^) "" [string_of_int a; ",[" ; string_of_int s1 ; "]->["; string_of_int s2; "]\n"] in 
  let print_state s = 
    List.fold_left (^) "" ["["; string_of_int s; "]\n"] in
      output_string ch (print_state 0);
      List.iter (fun x -> output_string ch (print_trans x)) l1;
      List.iter (fun x -> output_string ch (print_state x)) l2;
    close_out ch



(* types for the gff representation of automata *)
type transition = state * string * state

module StateSet = 
  Set.Make(struct type t=state let compare=compare end)
module TransitionSet = 
  Set.Make(struct type t=transition let compare=compare end)
type alphabet = (string,int) Hashtbl.t


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

let letter_int_association_list_of_gff_info gff_info =
  let res = ref [] in
  Hashtbl.iter (fun letter->fun integ->res:=(letter,integ)::!res) gff_info.alphabet;
  !res


let nba_of_gff_file filename =
  let xml_root = Xml.parse_file filename in
  let gff_info = gff_info_of_xml xml_root in
  { nba_array = automaton_array_of_gff_info gff_info;
    nba_letter_int_list = letter_int_association_list_of_gff_info gff_info;
  }


let gff_file_of_nba nba filename = 
  let a = nba.nba_array in
  let l = List.map (fun (x,y)->(y,x)) nba.nba_letter_int_list in
  let string_of_symbol i =
    List.assoc i l in
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
    Format.fprintf fmt "a%s@," (string_of_symbol i))
      done) in
  let pp_transitionset fmt =
    pp_node fmt "TransitionSet" [] (fun fmt->
      let tid = ref 0 in
      let pp src ltr dst =
  tid:=!tid+1;
  pp_node fmt "Transition" ["tid",string_of_int !tid] (fun fmt->
    Format.fprintf fmt 
      "<From>%i</From><To>%i</To><Label>a%s</Label>@,"
      src dst (string_of_symbol ltr)) in
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







