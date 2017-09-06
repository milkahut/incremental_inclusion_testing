type token =
  | ID of (string)
  | COMMA
  | ARROW
  | EOL
  | EOF

open Parsing;;
# 2 "ba_parser.mly"
let initial_state = ref None
let state_counter = ref 1
let state_list = ref [] 
let state string = 
  try 
    let p = List.find (fun x -> fst x = string) !state_list in
      snd p
  with 
    Not_found -> 
      let p = (string,!state_counter) in
        state_list := p::!state_list;
        incr state_counter;
        snd p
let all_state counter = 
  let rec list n = if n = 0 then [] else (n-1)::(list (n-1)) in
    list counter

let sym_counter = ref 0
let sym_list = ref [] 
let sym string = 
  try 
    let p = List.find (fun x -> fst x = string) !sym_list in
      snd p
  with 
    Not_found -> 
      let p = (string,!sym_counter) in
        sym_list := p::!sym_list;
        incr sym_counter;
        snd p 

let nba sym_size state_size transitions acc_states =
  let aut = Array.init state_size (fun q -> (Array.make sym_size [],false)) in
    List.iter (fun i -> aut.(i) <- (fst aut.(i), true)) acc_states;
    List.iter (fun (a,(i,j)) -> (fst aut.(i)).(a) <- j::(fst aut.(i)).(a)) transitions;
    aut
# 46 "ba_parser.ml"
let yytransl_const = [|
  258 (* COMMA *);
  259 (* ARROW *);
  260 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\005\000\002\000\002\000\004\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\003\000\006\000\002\000\001\000\002\000\
\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000\007\000\000\000\
\008\000\002\000\000\000\006\000\000\000\000\000\000\000\000\000\
\009\000\004\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\004\000\005\000\010\000\011\000\007\000"

let yysindex = "\001\000\
\005\255\000\000\001\255\000\000\005\255\006\255\000\000\007\255\
\000\000\000\000\009\255\000\000\010\255\005\255\008\255\011\255\
\000\000\000\000\012\255\013\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\013\000\246\255\019\000\251\255"

let yytablesize = 20
let yytable = "\012\000\
\017\000\001\000\008\000\018\000\009\000\003\000\013\000\015\000\
\012\000\016\000\019\000\008\000\020\000\001\000\009\000\010\000\
\021\000\003\000\014\000\006\000"

let yycheck = "\005\000\
\011\000\001\000\002\001\014\000\004\001\001\001\001\001\001\001\
\014\000\001\001\003\001\002\001\001\001\000\000\004\001\000\000\
\004\001\000\000\006\000\001\000"

let yynames_const = "\
  COMMA\000\
  ARROW\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition_list) in
    Obj.repr(
# 51 "ba_parser.mly"
    ( (nba !sym_counter !state_counter _1 (all_state !state_counter), !sym_list) )
# 116 "ba_parser.ml"
               : ((int list array * bool) array) * ((string * int) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transition_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_list) in
    Obj.repr(
# 53 "ba_parser.mly"
    ( (nba !sym_counter !state_counter _1 _2, !sym_list) )
# 124 "ba_parser.ml"
               : ((int list array * bool) array) * ((string * int) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition_list) in
    Obj.repr(
# 55 "ba_parser.mly"
    ( (nba !sym_counter !state_counter _2 (all_state !state_counter), !sym_list) )
# 132 "ba_parser.ml"
               : ((int list array * bool) array) * ((string * int) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'state) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transition_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'state_list) in
    Obj.repr(
# 57 "ba_parser.mly"
    ( (nba !sym_counter !state_counter _2 _3, !sym_list) )
# 141 "ba_parser.ml"
               : ((int list array * bool) array) * ((string * int) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 61 "ba_parser.mly"
    ( if !initial_state = None then 
        (let init = (_3,0) in
           initial_state := Some init; 
           state_list := init::!state_list);
      (sym _1,(state _3,state _5)) )
# 154 "ba_parser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transition_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 68 "ba_parser.mly"
                                 ( _2::_1 )
# 162 "ba_parser.ml"
               : 'transition_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 69 "ba_parser.mly"
                                 ( [_1] )
# 169 "ba_parser.ml"
               : 'transition_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 73 "ba_parser.mly"
    ( if !initial_state = None then 
        (let init = (_1,0) in
           initial_state := Some init; 
           state_list := init::!state_list);
      state _1 )
# 180 "ba_parser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_list) in
    Obj.repr(
# 80 "ba_parser.mly"
                                 ( _1::_2 )
# 188 "ba_parser.ml"
               : 'state_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 81 "ba_parser.mly"
                                 ( [_1] )
# 195 "ba_parser.ml"
               : 'state_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ((int list array * bool) array) * ((string * int) list))
