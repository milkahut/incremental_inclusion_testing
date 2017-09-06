type token =
  | INT of (int)
  | ANN of (string)
  | STRING of (string)
  | SEMICOLON
  | COMMA
  | EOF
  | TS
  | START

open Parsing;;
# 3 "src/automata/parser/tcstsparser.mly"

  open Tcsautomataparserinternal ;;
  
# 17 "src/automata/parser/tcstsparser.ml"
let yytransl_const = [|
  260 (* SEMICOLON *);
  261 (* COMMA *);
    0 (* EOF *);
  262 (* TS *);
  263 (* START *);
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
\002\000\002\000\002\000\003\000\003\000\003\000\004\000\001\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\000\000\020\000\000\000\003\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\013\000\000\000\014\000\
\004\000\000\000\002\000\007\000\017\000\019\000\015\000\005\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\018\000\009\000\013\000\014\000"

let yysindex = "\017\000\
\003\000\000\000\005\255\000\000\003\255\000\000\001\000\000\000\
\016\255\017\255\000\000\018\255\007\255\019\255\020\255\024\255\
\000\000\002\000\002\000\025\255\026\255\000\000\028\255\000\000\
\000\000\023\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\027\255\000\000\000\000\000\000\000\000\000\000\
\000\000\012\255\000\000\015\255\029\255\030\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\249\255\000\000\000\000\008\000\248\255"

let yytablesize = 265
let yytable = "\017\000\
\004\000\004\000\004\000\015\000\023\000\010\000\011\000\012\000\
\022\000\012\000\027\000\028\000\030\000\016\000\016\000\016\000\
\018\000\001\000\018\000\019\000\024\000\020\000\021\000\025\000\
\026\000\010\000\032\000\029\000\012\000\031\000\008\000\000\000\
\009\000\010\000\012\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\003\000\003\000\003\000\000\000\000\000\000\000\016\000\
\005\000"

let yycheck = "\007\000\
\000\000\000\000\000\000\001\001\013\000\001\001\002\001\003\001\
\002\001\003\001\018\000\019\000\021\000\002\001\003\001\004\001\
\002\001\001\000\004\001\004\001\002\001\005\001\005\001\004\001\
\001\001\001\001\004\001\020\000\003\001\002\001\004\001\255\255\
\004\001\004\001\004\001\255\255\255\255\255\255\255\255\255\255\
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
\255\255\001\001\001\001\001\001\255\255\255\255\255\255\007\001\
\006\001"

let yynames_const = "\
  SEMICOLON\000\
  COMMA\000\
  EOF\000\
  TS\000\
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
# 22 "src/automata/parser/tcstsparser.mly"
                                      ( )
# 161 "src/automata/parser/tcstsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 23 "src/automata/parser/tcstsparser.mly"
                                   ( )
# 170 "src/automata/parser/tcstsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 24 "src/automata/parser/tcstsparser.mly"
                                      ( )
# 177 "src/automata/parser/tcstsparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 27 "src/automata/parser/tcstsparser.mly"
                            ( !__ts_has_header _2 )
# 184 "src/automata/parser/tcstsparser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 30 "src/automata/parser/tcstsparser.mly"
                               ( !__ts_has_start _2 )
# 191 "src/automata/parser/tcstsparser.ml"
               : 'start))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "src/automata/parser/tcstsparser.mly"
                               ( )
# 197 "src/automata/parser/tcstsparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 34 "src/automata/parser/tcstsparser.mly"
                               ( )
# 205 "src/automata/parser/tcstsparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "src/automata/parser/tcstsparser.mly"
      ( !__ts_add_node _1 [] [] None )
# 212 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 39 "src/automata/parser/tcstsparser.mly"
                   ( !__ts_add_node _1 _2 [] None )
# 220 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 40 "src/automata/parser/tcstsparser.mly"
                    ( !__ts_add_node _1 [] _2 None )
# 228 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "src/automata/parser/tcstsparser.mly"
           ( !__ts_add_node _1 [] [] (Some _2) )
# 236 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 42 "src/automata/parser/tcstsparser.mly"
                                ( !__ts_add_node _1 _2 _3 None )
# 245 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "src/automata/parser/tcstsparser.mly"
                       ( !__ts_add_node _1 _2 [] (Some _3) )
# 254 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'propositions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "src/automata/parser/tcstsparser.mly"
                        ( !__ts_add_node _1 [] _2 (Some _3) )
# 263 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'propositions) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "src/automata/parser/tcstsparser.mly"
                                    ( !__ts_add_node _1 _2 _3 (Some _4) )
# 273 "src/automata/parser/tcstsparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "src/automata/parser/tcstsparser.mly"
           ( [ _1 ] )
# 280 "src/automata/parser/tcstsparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 50 "src/automata/parser/tcstsparser.mly"
                          ( _1::_3 )
# 288 "src/automata/parser/tcstsparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "src/automata/parser/tcstsparser.mly"
                                    ( [ _1 ] )
# 295 "src/automata/parser/tcstsparser.ml"
               : 'propositions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'propositions) in
    Obj.repr(
# 55 "src/automata/parser/tcstsparser.mly"
                                      ( _1::_3 )
# 303 "src/automata/parser/tcstsparser.ml"
               : 'propositions))
(* Entry ts *)
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
let ts (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
