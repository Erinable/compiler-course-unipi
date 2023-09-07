exception Syntax_error of Location.lexeme_pos * string

(* rename the arguments as you wish *)
let parse _scanner _lexbuf =
  try Parser.program _scanner _lexbuf with
  | Scanner.Lexing_error _ as sle -> raise sle
  | Syntax_error _ as spe -> raise spe
  | Parser.Error ->
      raise (Syntax_error (Location.to_lexeme_position _lexbuf, "Syntax error"))
