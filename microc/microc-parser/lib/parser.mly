/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
  open Ast
  let (|@|) node loc = Ast.annotate loc node
  let fex x loc= Ast.Stmt(Ast.Expr(x) |@| loc) |@| loc
  let f1 x loc = match x with
    | None -> None
    | Some x' -> Some (fex x' loc)

%}

/* Tokens declarations */
%token<string> ID
%token<int> INT
%token<string> STRING
%token<float> FLOAT
%token<char> CHAR
%token IF
%token ELSE
%token WHILE
%token DO
%token RETURN
%token FOR
%token INT_TYPE
%token FLOAT_TYPE
%token STRING_TYPE
%token BOOLEAN_TYPE
%token CHAR_TYPE
%token VOID
%token NULL
%token TRUE
%token FALSE
%token ADDRESS
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token ASSIGN
%token LESS
%token GREATER
%token LEQ
%token GEQ
%token EQUAL
%token NEQ
%token AND
%token OR
%token NOT
%token LPAREN
%token RPAREN
%token RBRACKET
%token LBRACKET
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token EOF

/* Precedence and associativity specification */
%nonassoc THEN
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUAL,NEQ
%nonassoc GREATER,LESS,GEQ,LEQ
%left PLUS,MINUS
%left TIMES,MOD,DIV
%nonassoc NOT,ADDRESS
%nonassoc LBRACKET

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  | t=list(topdecl) EOF              {Ast.Prog(t)}
;
topdecl:
  | v=vardecl_init_list { Ast.Vardec(fst v,snd v) |@| $loc}
  | f=fundecl {Ast.Fundecl(f) |@| $loc}
;
vardecl_init_list:
  | vardecl SEMICOLON {$1}
;
typ:
  | INT_TYPE  {Ast.TypI}
  | STRING_TYPE {Ast.TypS}
  | FLOAT_TYPE  {Ast.TypF}
  | BOOLEAN_TYPE  {Ast.TypB}
  | CHAR_TYPE {Ast.TypC}
  | VOID {Ast.TypV}
;
vardecl:
  | t=typ v=vardesc {let vl = ref "he" in Ast.generate_vardecl t v vl}
;
vardesc:
  | i=ID {[Ast.TypID2(i)]}
  | TIMES v=vardesc {Ast.TypID1("*")::v}
  | LPAREN v=vardesc RPAREN {v}
  | v=vardesc LBRACKET RBRACKET {v @ [Ast.TypID1("a")]}
  | v=vardesc LBRACKET i=INT RBRACKET {v @ [Ast.TypID1(string_of_int i)]}
;
fundecl:
  | t=typ id=ID LPAREN v=separated_list(COMMA,vardecl) RPAREN b=block { {typ=t;fname=id;formals=v;body=b} }
;
block:
  | LBRACE s=list(stmtordec) RBRACE { Ast.Block(s) |@| $loc}
;
stmtordec:
  | s=stmt {Ast.Stmt(s) |@| $loc}
  | v=vardecl_init_list {Ast.Dec(fst v, snd v) |@| $loc}
;
stmt:
  | RETURN e=option(expr) SEMICOLON { Ast.Return(e) |@| $loc }
  | option(expr) SEMICOLON
    {
      match $1 with
        | None -> Ast.Block([]) |@| $loc
        | Some(ex) -> Ast.Expr(ex) |@| $loc
    }
  | b=block  { b }
  | WHILE LBRACE e=expr RBRACE s=stmt {Ast.While(e,s) |@| $loc}
  | DO s=stmt WHILE LBRACE e=expr RBRACE SEMICOLON
    {
      Ast.Block([Ast.Stmt(s) |@| $loc(s);Ast.Stmt(Ast.While(e,s) |@| $loc) |@| $loc]) |@| $loc
    }
  | FOR LBRACE e1=option(expr) SEMICOLON option(expr) SEMICOLON e3=option(expr) RBRACE s=stmt
    {
      let init_expr_stmt =
        let loc_e = ($startpos(e1),$endpos($4)) in
        f1 e1 loc_e
      in
      let update_expr_stmt =
        f1 e3 $loc(e3)
      in
      let while_stmt =
        match update_expr_stmt with
          | None -> s
          | Some(uestm) -> Ast.Block([Ast.Stmt(s) |@| $loc(s);uestm]) |@| ($startpos(s), $endpos(s))
      in
      let while_expr =
        match e1 with
          | None -> Ast.BLiteral(true) |@| $loc(e1)
          | Some(ex) -> ex
      in
      let while_partial = Ast.While(while_expr,while_stmt)
      in
      let while_partial_loc = ($startpos($5), $endpos(s))
      in
      let for_as_while =
        match init_expr_stmt with
          | None -> while_partial |@| while_partial_loc
          | Some(iestm) -> Ast.Block([iestm;Ast.Stmt(while_partial |@| while_partial_loc) |@| while_partial_loc]) |@| $loc
      in
      for_as_while
    }
  | IF LBRACE e=expr RBRACE s1=stmt ELSE s2=stmt
    {
      Ast.If(e,s1,s2) |@| $loc
    }
  | IF LBRACE e=expr RBRACE s1=stmt %prec THEN
    {
      Ast.If(e,s1,Ast.Block([]) |@| ($endpos(s1),$endpos(s1))) |@| $loc
    }
  /*| IF LBRACE e=expr RBRACE s=stmt {Ast.If(e,s,None)}*/
;
expr:
  | e=rexpr {e}
  | e=lexpr_access {e}
;
%inline lexpr_access:
  | lexpr { Ast.Access($1) |@| $loc }
;
aexpr:
  | i=INT {Ast.ILiteral(i) |@| $loc}
  | c=CHAR {Ast.CLiteral(c) |@| $loc}
  | TRUE {Ast.BLiteral(true) |@| $loc}
  | FALSE {Ast.BLiteral(false) |@| $loc}
  | f=FLOAT {Ast.FLiteral(f) |@| $loc}
  | s=STRING {Ast.SLiteral(s) |@| $loc}
  | NULL {Ast.Null |@| $loc}
  | LBRACE e=rexpr RBRACE {e}
  | ADDRESS e=lexpr {Ast.Addr(e) |@| $loc}
;
lexpr:
  | id=ID {Ast.AccVar(id) |@| $loc}
  | TIMES e=lexpr_access {Ast.AccDeref(e) |@| $loc}
  | TIMES e=aexpr {Ast.AccDeref(e) |@| $loc}
  | e1=lexpr LBRACKET e2=expr RBRACKET {Ast.AccIndex(e1,e2) |@| $loc}
;
rexpr:
  | e=aexpr {e}
  | id=ID LBRACE s=separated_list(COMMA,expr) RBRACE { Ast.Call(id,s) |@| $loc }
  | e1=lexpr ASSIGN e2=expr { Ast.Assign(e1,e2) |@| $loc }
  | uop e=expr {Ast.UnaryOp($1,e) |@| $loc}
  | e1=expr b=binop e2=expr { Ast.BinaryOp(b,e1,e2) |@| $loc}
;

%inline binop:
  | PLUS {Ast.Add}
  | MINUS {Ast.Sub}
  | TIMES {Ast.Mult}
  | MOD {Ast.Mod}
  | DIV {Ast.Div}
  | AND {Ast.And}
  | OR {Ast.Or}
  | GREATER {Ast.Greater}
  | LESS {Ast.Less}
  | GEQ {Ast.Geq}
  | LEQ {Ast.Leq}
  | EQUAL {Ast.Equal}
  | NEQ {Ast.Neq}
;
%inline uop:
  | NOT {Ast.Not}
  | MINUS {Ast.Minus}

