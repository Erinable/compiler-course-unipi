{
  open Parser

  exception Error of string

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table =
    create_hashtable 8 [
      ("let", LET);
      ("sin", SIN);
      ("cos", COS);
      ("log", LOG);
    ]

}

let digit = ['0'-'9']
let one_to_nine = ['1'-'9']
let int = ('-'? one_to_nine digit*) | '0'

let frac = '.' digit*
let exp = ['e''E'] ['-''+']? digit+
let float = digit* frac? exp?

let white = [' ''\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| id as word
    {try (Hashtbl.find keyword_table word) with Not_found -> ID(word)}
| int
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '='
    { EQ }
| float as ft
    { FLOAT ( float_of_string ft) }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

