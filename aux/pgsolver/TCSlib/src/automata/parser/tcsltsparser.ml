type token =
  | INT of (int)
  | ANN of (string)
  | STRING of (string)
  | SEMICOLON
  | COMMA
  | COLON
  | EOF
  | LTS
  | START

open Parsing;;
# 3 "src/automata/parser/tcsltsparser.mly"

  open Tcsautomataparserinternal ;;
  
# 18 "src/automata/parser/tcsltsparser.ml"
let yytransl_const = [|
  260 (* SEMICOLON *);
  261 (* COMMA *);
  262 (* COLON *);
    0 (* EOF *);
  263 (* LTS *);
  264 (* START *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ANN *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\004\000\003\000\003\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\002\000\002\000\002\000\003\000\003\000\003\000\004\000\003\000\
\005\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\000\000\020\000\000\000\003\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\014\000\
\004\000\000\000\002\000\007\000\019\000\000\000\015\000\005\000\
\000\000\000\000\017\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\017\000\009\000\012\000\013\000"

let yysindex = "\014\000\
\003\000\000\000\002\255\000\000\019\255\000\000\001\000\000\000\
\017\255\000\000\011\255\016\255\020\255\021\255\022\255\000\000\
\002\000\002\000\023\255\026\255\000\000\024\255\028\255\000\000\
\000\000\027\255\000\000\000\000\000\000\029\255\000\000\000\000\
\025\255\018\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\031\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\255\032\255\033\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\255\034\255\000\000\
\000\000\000\000\000\000\000\000\000\000\005\255\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\249\255\000\000\000\000\255\255\250\255"

let yytablesize = 266
let yytable = "\016\000\
\004\000\004\000\004\000\010\000\011\000\023\000\016\000\016\000\
\016\000\027\000\028\000\018\000\029\000\018\000\001\000\019\000\
\020\000\021\000\022\000\014\000\018\000\024\000\026\000\020\000\
\025\000\022\000\030\000\034\000\019\000\031\000\032\000\035\000\
\000\000\033\000\008\000\009\000\010\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
\015\000\005\000"

let yycheck = "\007\000\
\000\000\000\000\000\000\002\001\003\001\012\000\002\001\003\001\
\004\001\017\000\018\000\002\001\019\000\004\001\001\000\005\001\
\006\001\002\001\003\001\001\001\004\001\002\001\001\001\006\001\
\004\001\003\001\001\001\003\001\005\001\002\001\004\001\033\000\
\255\255\005\001\004\001\004\001\004\001\004\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\001\001\001\001\255\255\255\255\255\255\255\255\
\008\001\007\001"

let yynames_const = "\
  SEMICOLON\000\
  COMMA\000\
  COLON\000\
  EOF\000\
  LTS\000\
  START\000\
  "

let yynames_block = "\
  INT\000\
  ANN\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 23 "src/automata/parser/tcsltsparser.mly"
                                      ( )
# 167 "src/automata/parser/tcsltsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 24 "src/automata/parser/tcsltsparser.mly"
                                   ( )
# 176 "src/automata/parser/tcsltsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 25 "src/automata/parser/tcsltsparser.mly"
                                      ( )
# 183 "src/automata/parser/tcsltsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 28 "src/automata/parser/tcsltsparser.mly"
                             ( !__lts_has_header _2 )
# 190 "src/automata/parser/tcsltsparser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 31 "src/automata/parser/tcsltsparser.mly"
                               ( !__lts_has_start _2 )
# 197 "src/automata/parser/tcsltsparser.ml"
               : 'start))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "src/automata/parser/tcsltsparser.mly"
                               ( )
# 203 "src/automata/parser/tcsltsparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 35 "src/automata/parser/tcsltsparser.mly"
                               ( )
# 211 "src/automata/parser/tcsltsparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "src/automata/parser/tcsltsparser.mly"
      ( !__lts_add_node _1 [] [] None )
# 218 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 40 "src/automata/parser/tcsltsparser.mly"
                   ( !__lts_add_node _1 _2 [] None )
# 226 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 41 "src/automata/parser/tcsltsparser.mly"
                    ( !__lts_add_node _1 [] _2 None )
# 234 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "src/automata/parser/tcsltsparser.mly"
           ( !__lts_add_node _1 [] [] (Some _2) )
# 242 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 43 "src/automata/parser/tcsltsparser.mly"
                                ( !__lts_add_node _1 _2 _3 None )
# 251 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "src/automata/parser/tcsltsparser.mly"
                       ( !__lts_add_node _1 _2 [] (Some _3) )
# 260 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'propositions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "src/automata/parser/tcsltsparser.mly"
                        ( !__lts_add_node _1 [] _2 (Some _3) )
# 269 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'propositions) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "src/automata/parser/tcsltsparser.mly"
                                    ( !__lts_add_node _1 _2 _3 (Some _4) )
# 279 "src/automata/parser/tcsltsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "src/automata/parser/tcsltsparser.mly"
                        ( [ (_1,_3) ] )
# 287 "src/automata/parser/tcsltsparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 51 "src/automata/parser/tcsltsparser.mly"
                                       ( (_1,_3)::_5 )
# 296 "src/automata/parser/tcsltsparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "src/automata/parser/tcsltsparser.mly"
                                    ( [ _1 ] )
# 303 "src/automata/parser/tcsltsparser.ml"
               : 'propositions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 56 "src/automata/parser/tcsltsparser.mly"
                                      ( _1::_3 )
# 311 "src/automata/parser/tcsltsparser.ml"
               : 'propositions))
(* Entry lts *)
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
let lts (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
