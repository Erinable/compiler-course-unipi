let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    let v = Ast.eval (Parser.main Lexer.token linebuf) in
    match v with
    | ("-", FLOAT i) -> Printf.printf "%2f\n%!" i
    | ("-", INT i) -> Printf.printf "%d\n%!" i
    | (j,FLOAT i) -> Printf.printf "val %s : %2f\n%!" j i
    | (j,INT i) -> Printf.printf "val %s : %d\n%!" j i
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset1 %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (Lexing.from_channel stdin)

