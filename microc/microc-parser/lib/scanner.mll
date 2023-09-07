{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let create_hashtable size init =
      let tbl = Hashtbl.create size in
      List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
      tbl

    let keyword_table =
      create_hashtable 14 [
        ("if",IF);
        ("else",ELSE);
        ("while",WHILE);
        ("return",RETURN);
        ("for",FOR);
        ("do",DO);
        ("int",INT_TYPE);
        ("float",FLOAT_TYPE);
        ("string",STRING_TYPE);
        ("bool",BOOLEAN_TYPE);
        ("char",CHAR_TYPE);
        ("void",VOID);
        ("NULL",NULL);
        ("true", TRUE);
        ("false", FALSE)
    ]
    let char_for_backslash = function
      | 'n' -> '\010'
      | 't' -> '\009'
      | 'b' -> '\008'
      | 'r' -> '\013'
      | 'f' -> '\012'
      | c   -> c

}

(* Scanner specification *)
let id = ['a'-'z' 'A' - 'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let int1 = '-'? ((['1'-'9'] digit*) | '0')
let int2 = '0' 'x' ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f']
let int = int1 | int2
let exp = ['e' 'E'] ['-' '+']? digit+
let float1 = int1 '.' digit* exp?
let float2 = '.' digit+ exp?
let float = float1 | float2
let white = [' ' '\t']
let backslash_escapes =['\\' '\'' '"' 'n' 't' 'b' 'r' 'f']

rule next_token = parse
  | white                  { next_token lexbuf }
  | '\n'                   { Lexing.new_line lexbuf;next_token lexbuf }
  | id as word             { try Hashtbl.find keyword_table word with Not_found -> ID(word) }
  | int as num             { let integer = int_of_string num in INT(integer) }
  | float as fnum          { let floatn = float_of_string fnum in FLOAT(floatn) }
  | '"'                    { read_string (Buffer.create 17) lexbuf }
  | "'" [^ '\\'] "'"       { CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' backslash_escapes "'"
                           { CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "//" [^'\n']*          { next_token lexbuf }
  | "/*"                   { read_comments lexbuf }
  | '&'                    { ADDRESS }
  | '+'                    { PLUS }
  | '-'                    { MINUS }
  | '*'                    { TIMES }
  | '/'                    { DIV }
  | '%'                    { MOD }
  | '='                    { ASSIGN }
  | '<'                    { LESS }
  | '>'                    { GREATER }
  | "<="                   { LEQ }
  | ">="                   { GEQ }
  | "=="                   { EQUAL }
  | "!="                   { NEQ }
  | "&&"                   { AND }
  | "||"                   { OR }
  | '!'                    { NOT }
  | '('                    { LPAREN }
  | ')'                    { RPAREN }
  | ']'                    { RBRACKET }
  | '['                    { LBRACKET }
  | '{'                    { LBRACE }
  | '}'                    { RBRACE }
  | ';'                    { SEMICOLON }
  | ','                    { COMMA }
  | eof                    { EOF }
  | _                      { raise (Lexing_error (Location.to_lexeme_position lexbuf, "Unexpected character: " ^ (Lexing.lexeme lexbuf))) }
and read_string buffer = parse
  | '"'                     { STRING( Buffer.contents buffer ) }
  | '\\' '/'                { Buffer.add_char buffer '/'; read_string buffer lexbuf }
  | '\\' '\\'               { Buffer.add_char buffer '\\'; read_string buffer lexbuf }
  | '\\' 'b'                { Buffer.add_char buffer '\b'; read_string buffer lexbuf }
  | '\\' 'f'                { Buffer.add_char buffer '\012'; read_string buffer lexbuf }
  | '\\' 'n'                { Buffer.add_char buffer '\n'; read_string buffer lexbuf }
  | '\\' 'r'                { Buffer.add_char buffer '\r'; read_string buffer lexbuf }
  | '\\' 't'                { Buffer.add_char buffer '\t'; read_string buffer lexbuf }
  | [^'"' '\\']+            { Buffer.add_string buffer (Lexing.lexeme lexbuf); read_string buffer lexbuf }
  | eof                     { raise (Lexing_error (Location.to_lexeme_position lexbuf,"Literal string is not terminated")) }
  | _                       { raise (Lexing_error (Location.to_lexeme_position lexbuf,"Illegal string character: " ^ Lexing.lexeme lexbuf)) }
and read_comments = parse
  | "*/"                    { next_token lexbuf }
  | _                       { read_comments lexbuf }
  | eof                     { raise (Lexing_error (Location.to_lexeme_position lexbuf,"comments is not terminated")) }