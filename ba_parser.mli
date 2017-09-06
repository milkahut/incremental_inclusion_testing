type token =
  | ID of (string)
  | COMMA
  | ARROW
  | EOL
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((int list array * bool) array) * ((string * int) list)
