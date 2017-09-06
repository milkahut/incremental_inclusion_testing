(* File tcsparitygamelexer.mll *)

{

open Tcsparitygameparser ;;        (* The type token is defined in tcsparitygameparser.mli *)
open Tcsautomataparserinternal ;;

}

rule token = parse
    ['\n']                             { incr __lexer_line; __lexer_character := 0; token lexbuf }
  | [' ' '\t']                         { incr __lexer_character; token lexbuf }
  | ['0'-'9']+ as lxm                  { __lexer_character := !__lexer_character + (String.length lxm); INT(int_of_string lxm) }
  | ','                                { incr __lexer_character; COMMA }
  | ';'                                { incr __lexer_character; SEMICOLON }
  | '"'((_ # ['"' '\n'])* as lxm)'"'   { __lexer_character := !__lexer_character + (String.length lxm) + 2; ANN(lxm) }
  | eof                                { incr __lexer_character; EOF }
  | "parity"                           { __lexer_character := !__lexer_character + 6; PARITY }
  | "start"                            { __lexer_character := !__lexer_character + 5; START }
