%{
  open Ast

%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LET EQ SIN COS LOG
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Ast.expr> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { Int i }
| id = ID
    { Var id}
| i = FLOAT
    { Float i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { Prim("+",e1,e2) }
| e1 = expr MINUS e2 = expr
    { Prim("-",e1,e2) }
| e1 = expr TIMES e2 = expr
    { Prim("*",e1,e2) }
| e1 = expr DIV e2 = expr
    { Prim("/",e1,e2) }
| SIN LPAREN e1 = expr RPAREN
    { Prim("sin",e1,Float 0.) }
| COS LPAREN e1 = expr RPAREN
    { Prim("cos",e1,Float 0.) }
| LOG LPAREN e1 = expr RPAREN
    { Prim("log",e1,Float 0.) }
| MINUS e = expr %prec UMINUS
    { Prim("-",Int 0,e) }
| LET id=ID EQ e=expr
    {LET(id, e)}

