{
type token_type = PLUS | IF | ID of string | NUM of int | EOF | FLOAT of string | STRING of string | COMMENT of string
and token = { pos : Lexing.position ; tok_type : token_type; tok_length:int }

exception Error of string

let string_buff = Buffer.create 256
let comments_total_char = ref 0
let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let string_of_token_type = function
  | PLUS   -> "PLUS"
  | IF     -> "IF"
  | FLOAT(s) -> Printf.sprintf "FLOAT(%s)" s
  | STRING(s) -> Printf.sprintf "STRING(%s)" s
  | ID(s)  -> Printf.sprintf "ID(%s)" s
  | NUM(i) -> Printf.sprintf "NUM(%d)" i
  | EOF    -> "eof"
  | COMMENT(s) -> Printf.sprintf "COMMENT(%s)" s

let string_of_position p =
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Printf.sprintf "(%d, %d)" line_number column

let rec iterate scanner =
  match scanner () with
  | { tok_type=EOF; pos=p;tok_length = _ } -> Printf.printf "total line %d,total char %d,total comments char %d, comment-density %.2f\n" p.Lexing.pos_lnum p.Lexing.pos_cnum !comments_total_char ((float_of_int !comments_total_char) /. (float_of_int p.Lexing.pos_cnum))
  | tok -> Printf.printf "%s @ %s @ %n\n" (string_of_token_type tok.tok_type) (string_of_position tok.pos) tok.tok_length; iterate scanner

let make_token ttype lexbuf = { tok_type = ttype ; pos = Lexing.lexeme_start_p lexbuf; tok_length = (Lexing.lexeme_end_p lexbuf).pos_cnum - (Lexing.lexeme_start_p lexbuf).pos_cnum }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = letter (letter | digit | '_')*
let float = ('-' digit+ | digit* ) ('.')? (digit+ (('E' | 'e')('+' | '-')? digit+)?)?
let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule token = parse
  | '\n'                     { Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t']               { token lexbuf } (* ignore whitespace *)
  | "(*"                     { Buffer.clear string_buff;comments 0 (Lexing.lexeme_start_p lexbuf) lexbuf;}
  | '"'                      { Buffer.clear string_buff;strings 0 (Lexing.lexeme_start_p lexbuf) lexbuf; }
  | float as ft              { make_token (FLOAT(ft)) lexbuf}
  | '+'                      { make_token PLUS lexbuf }       (* a symbol *)
  | "if"                     { make_token IF  lexbuf  }       (* a keyword *)
  | identifier as id         { make_token (ID(id)) lexbuf }   (* identifiers *)
  | digit+ as lit            { make_token (NUM(int_of_string lit)) lexbuf } (* numeric literals *)
  | eof                      { make_token EOF lexbuf }
  | _ as d { raise (Error("Unexpected token:" ^ (Char.escaped d)))}
and strings n pos = parse
  | '"' { {tok_type=STRING(Buffer.contents string_buff);pos=pos;tok_length=(n+0)} }
  | '\\' (backslash_escapes as c) {Buffer.add_char string_buff (char_for_backslash c);strings (n+1) pos lexbuf }
  | _ as c {Buffer.add_char string_buff c;strings (n+1) pos lexbuf}
and comments n pos = parse
  | "*)" {comments_total_char:= n + !comments_total_char;{tok_type=COMMENT(Buffer.contents string_buff);pos=pos;tok_length=n}}
  | _ as c {Buffer.add_char string_buff c;comments (n+1) pos lexbuf}

{
let () =
  let lexbuf = Lexing.from_channel ~with_positions:true stdin in
  iterate (fun () -> token lexbuf)
}