type token =
  | INT of (int)
  | SEMICOLON
  | EOF
  | PARITYSOL

val sol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
