type token =
  | INT of (int)
  | ANN of (string)
  | SEMICOLON
  | COMMA
  | EOF
  | PARITY
  | START

open Parsing;;
# 3 "src/automata/parser/tcsparitygameparser.mly"

  open Tcsautomataparserinternal ;;
  
  let parse_player pl =
	if pl >= 0 && pl <= 1 then pl
	else (!__parse_exception ("Unknown Player: " ^ string_of_int pl); pl)
	
# 20 "src/automata/parser/tcsparitygameparser.ml"
let yytransl_const = [|
  259 (* SEMICOLON *);
  260 (* COMMA *);
    0 (* EOF *);
  261 (* PARITY *);
  262 (* START *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ANN *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\004\000\003\000\003\000\005\000\
\005\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\003\000\001\000\003\000\004\000\
\005\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\000\000\012\000\000\000\003\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\004\000\000\000\002\000\007\000\000\000\000\000\005\000\000\000\
\009\000\011\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\014\000\009\000\022\000"

let yysindex = "\005\000\
\003\000\000\000\008\255\000\000\009\255\000\000\001\000\000\000\
\010\255\011\255\012\255\013\255\000\000\002\000\002\000\015\255\
\000\000\014\255\000\000\000\000\007\255\016\255\000\000\015\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\255\017\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\249\255\000\000\000\000\251\255"

let yytablesize = 264
let yytable = "\013\000\
\004\000\004\000\004\000\010\000\010\000\001\000\019\000\020\000\
\010\000\011\000\024\000\016\000\015\000\018\000\017\000\021\000\
\023\000\025\000\026\000\008\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\003\000\003\000\000\000\000\000\012\000\005\000"

let yycheck = "\007\000\
\000\000\000\000\000\000\002\001\003\001\001\000\014\000\015\000\
\001\001\001\001\004\001\001\001\003\001\001\001\003\001\001\001\
\003\001\002\001\024\000\003\001\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\001\001\001\001\255\255\255\255\006\001\005\001"

let yynames_const = "\
  SEMICOLON\000\
  COMMA\000\
  EOF\000\
  PARITY\000\
  START\000\
  "

let yynames_block = "\
  INT\000\
  ANN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 25 "src/automata/parser/tcsparitygameparser.mly"
                                      ( )
# 158 "src/automata/parser/tcsparitygameparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 26 "src/automata/parser/tcsparitygameparser.mly"
                                   ( )
# 167 "src/automata/parser/tcsparitygameparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 27 "src/automata/parser/tcsparitygameparser.mly"
                                      ( )
# 174 "src/automata/parser/tcsparitygameparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 30 "src/automata/parser/tcsparitygameparser.mly"
                                ( !__pg_has_header _2 )
# 181 "src/automata/parser/tcsparitygameparser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 33 "src/automata/parser/tcsparitygameparser.mly"
                               ( !__pg_has_start _2 )
# 188 "src/automata/parser/tcsparitygameparser.ml"
               : 'start))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "src/automata/parser/tcsparitygameparser.mly"
                               ( )
# 194 "src/automata/parser/tcsparitygameparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nodelist) in
    Obj.repr(
# 37 "src/automata/parser/tcsparitygameparser.mly"
                               ( )
# 202 "src/automata/parser/tcsparitygameparser.ml"
               : 'nodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'succs) in
    Obj.repr(
# 41 "src/automata/parser/tcsparitygameparser.mly"
                             ( !__pg_add_node _1 _2 (parse_player _3) _4 "" )
# 212 "src/automata/parser/tcsparitygameparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'succs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "src/automata/parser/tcsparitygameparser.mly"
                             ( !__pg_add_node _1 _2 (parse_player _3) _4 _5 )
# 223 "src/automata/parser/tcsparitygameparser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "src/automata/parser/tcsparitygameparser.mly"
                            ( [ _1 ] )
# 230 "src/automata/parser/tcsparitygameparser.ml"
               : 'succs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'succs) in
    Obj.repr(
# 47 "src/automata/parser/tcsparitygameparser.mly"
                            ( _1::_3 )
# 238 "src/automata/parser/tcsparitygameparser.ml"
               : 'succs))
(* Entry game *)
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
let game (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
