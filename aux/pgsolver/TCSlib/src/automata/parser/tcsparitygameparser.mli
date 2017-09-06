type token =
  | INT of (int)
  | ANN of (string)
  | SEMICOLON
  | COMMA
  | EOF
  | PARITY
  | START

val game :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
