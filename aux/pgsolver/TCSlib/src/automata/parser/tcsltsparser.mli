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

val lts :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
