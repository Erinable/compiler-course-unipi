(* + - * / % = != < <= > >= && || *)
type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
[@@deriving show]

type uop = Minus | Not [@@deriving show]
type identifier = string [@@deriving show]

type 'a annotated_node = { loc : Location.code_pos; [@opaque] node : 'a }
[@@deriving show]

let annotate (a : Lexing.position * Lexing.position) v =
  { loc = Location.to_code_position a; node = v }

type typ =
  | TypI (* Type int *)
  | TypB (* Type bool *)
  | TypC (* Type char *)
  | TypA of typ * int option (* Array type *)
  | TypP of typ (* Pointer type  *)
  | TypV (* Type void  *)
  | TypS
  | TypF
  | TypID1 of identifier
  | TypID2 of string
[@@deriving show]

and expr = expr_node annotated_node

and expr_node =
  | Access of access (* x  or  *p  or  a[e]  *)
  | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of access (* &x   or  &*p   or  &a[e]  *)
  | ILiteral of int (* Integer literal  *)
  | CLiteral of char (* Char literal    *)
  | BLiteral of bool (* Bool literal    *)
  | FLiteral of float (* Float literal *)
  | SLiteral of string (* String literal *)
  | Null (* Null literal *)
  | UnaryOp of uop * expr (* Unary primitive operator  *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator  *)
  | Call of identifier * expr list (* Function call f(...)    *)
  | Comma of expr list
[@@deriving show]

and access = access_node annotated_node

and access_node =
  | AccVar of identifier (* Variable access    x  *)
  | AccDeref of expr (* Pointer dereferencing  *p *)
  | AccIndex of access * expr (* Array indexing   a[e] *)
[@@deriving show]

and stmt = stmt_node annotated_node

and stmt_node =
  | If of expr * stmt * stmt (* Conditional    *)
  | While of expr * stmt (* While loop     *)
  | For of expr option * expr option * expr option * stmt (* For loop *)
  | Expr of expr (* Expression statement   e;  *)
  | Return of expr option (* Return statement  *)
  | Block of stmtordec list (* Block: grouping and scope *)
[@@deriving show]

and stmtordec = stmtordec_node annotated_node

and stmtordec_node =
  | Dec of typ * identifier (* Local variable declaration  *)
  | Stmt of stmt (* A statement   *)
[@@deriving show]

type fun_decl = {
  typ : typ;
  fname : string;
  formals : (typ * identifier) list;
  body : stmt;
}
[@@deriving show]

type topdecl = topdecl_node annotated_node

and topdecl_node = Fundecl of fun_decl | Vardec of typ * identifier
[@@deriving show]

type program = Prog of topdecl list [@@deriving show]

let rec generate_type (base_type : typ) (tdl : typ list) vl =
  match tdl with
  | [] -> base_type
  | x :: xs -> (
      match x with
      | TypID1 "*" -> TypP (generate_type base_type xs vl)
      | TypID1 "a" -> TypA (generate_type base_type xs vl, None)
      | TypID1 p -> TypA (generate_type base_type xs vl, Some (int_of_string p))
      | TypID2 p ->
          vl := p;
          generate_type base_type xs vl
      | _ -> failwith "generate_type error")

let generate_vardecl base_type tdl vl =
  let t = generate_type base_type tdl vl in
  let v = !vl in
  (t, v)

