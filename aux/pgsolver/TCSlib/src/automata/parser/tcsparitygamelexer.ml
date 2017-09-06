# 3 "src/automata/parser/tcsparitygamelexer.mll"
 

open Tcsparitygameparser ;;        (* The type token is defined in tcsparitygameparser.mli *)
open Tcsautomataparserinternal ;;


# 9 "src/automata/parser/tcsparitygamelexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\000\000\000\000\249\255\001\000\251\255\252\255\012\000\
    \254\255\255\255\250\255\000\000\000\000\001\000\000\000\248\255\
    \001\000\004\000\003\000\247\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\255\255\255\255\255\255\000\000\004\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\009\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\004\000\010\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\000\000\005\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\017\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\012\000\001\000\016\000\014\000\018\000\019\000\
    \000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \003\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\002\000\016\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\012\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\011\000\000\000\001\000\013\000\017\000\018\000\
    \255\255\014\000\255\255\255\255\255\255\255\255\255\255\255\255\
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
    \000\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 11 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( incr __lexer_line; __lexer_character := 0; token lexbuf )
# 114 "src/automata/parser/tcsparitygamelexer.ml"

  | 1 ->
# 12 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( incr __lexer_character; token lexbuf )
# 119 "src/automata/parser/tcsparitygamelexer.ml"

  | 2 ->
let
# 13 "src/automata/parser/tcsparitygamelexer.mll"
                  lxm
# 125 "src/automata/parser/tcsparitygamelexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 13 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( __lexer_character := !__lexer_character + (String.length lxm); INT(int_of_string lxm) )
# 129 "src/automata/parser/tcsparitygamelexer.ml"

  | 3 ->
# 14 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( incr __lexer_character; COMMA )
# 134 "src/automata/parser/tcsparitygamelexer.ml"

  | 4 ->
# 15 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( incr __lexer_character; SEMICOLON )
# 139 "src/automata/parser/tcsparitygamelexer.ml"

  | 5 ->
let
# 16 "src/automata/parser/tcsparitygamelexer.mll"
                             lxm
# 145 "src/automata/parser/tcsparitygamelexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 16 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( __lexer_character := !__lexer_character + (String.length lxm) + 2; ANN(lxm) )
# 149 "src/automata/parser/tcsparitygamelexer.ml"

  | 6 ->
# 17 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( incr __lexer_character; EOF )
# 154 "src/automata/parser/tcsparitygamelexer.ml"

  | 7 ->
# 18 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( __lexer_character := !__lexer_character + 6; PARITY )
# 159 "src/automata/parser/tcsparitygamelexer.ml"

  | 8 ->
# 19 "src/automata/parser/tcsparitygamelexer.mll"
                                       ( __lexer_character := !__lexer_character + 5; START )
# 164 "src/automata/parser/tcsparitygamelexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

