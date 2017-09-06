type token =
  | INT of (int)
  | ANN of (string)
  | STRING of (string)
  | SEMICOLON
  | COMMA
  | EOF
  | TS
  | START

val ts :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
