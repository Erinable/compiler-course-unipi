(**
   An example of an interpreter for a small strongly typed functional language.
 *)
(** Syntax of the language *)

type expr =
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr
  | Tuple of expr * expr
  | List of expr * expr
  | Iop of string * expr
  | Empty

(** Definition of environment. An environment is a map from identifier to "something".
   In the semantics this something is a value (what the identifier is bound to).
   In the type system this "something" is a type.
   For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
 *)

type 'v env = (string * 'v) list

(**
   Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
   If there is no binding, it raises an exception.
 *)
let rec lookup env x =
    match env with
    | []        -> failwith (x ^ " not found")
    | (y, v)::r -> if x=y then v else lookup r x

(**
 Expressible and Denotable values.
 A runtime value is an integer or a function closure
 Boolean are encoded as integers.
*)
type value =
  | Int of int
  | Closure of string * string * expr * value env       (* (f, x, fBody, fDeclEnv) *)
  | Tup of value * value
  | Lis of value * value
  | Emp

(** Interpreter for expression. Given an expression {e} and an enviroment {env} that closes {e},
   this function evaluates {e} and returns the result of the computation.
   Note this function implements the big-step operational semantics with environment.
 *)

let rec proj tup n = match tup,n with
  | Tup(x,y),1 -> x
  | Tup(x,y),_ -> proj y (n-1)
  | Int x,_ -> Int x
  | _ -> failwith "invalid tup"

let rec eval (e : expr) (env : value env) : value =
    match e with
    | Empty -> Emp
    | CstI i -> Int i
    | CstB b -> Int (if b then 1 else 0)
    | Var x  -> lookup env x
    | Tuple(x,y) -> Tup(eval x env, eval y env)
    | List(x,y) -> Lis (eval x env, eval y env)
    | Iop(x,y) ->
      let v = eval y env in
      begin
      match (x,v) with
       | (_ , Lis(Emp,_)) -> failwith "list head is empty"
       | ("head", Lis(a,_)) -> a
       | ("tail", Lis(_,b)) -> b
       | _ -> failwith "unknown primitive or wrong type"
      end
    | Prim(ope, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      begin
      match (ope, v1, v2) with
      | ("*", Int i1, Int i2) -> Int (i1 * i2)
      | ("+", Int i1, Int i2) -> Int (i1 + i2)
      | ("-", Int i1, Int i2) -> Int (i1 - i2)
      | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
      | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
      | ("proj", tup, Int n) -> proj tup n
      | ("cons", a, (Lis(_,_) as b)) -> Lis(a, b)
      |  _ -> failwith "unknown primitive or wrong type"
      end
    | Let(x, eRhs, letBody) ->
      let xVal = eval eRhs env in
      let letEnv = (x, xVal) :: env in
      eval letBody letEnv
    | If(e1, e2, e3) ->
      begin
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"
      end
    | Letfun(f, x, fBody, letBody) ->
      let bodyEnv = (f, Closure(f, x, fBody, env)) :: env in
      eval letBody bodyEnv
    | Call(eFun, eArg) ->
      let fClosure = eval eFun env in
      begin
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVal = eval eArg env in
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
        in eval fBody fBodyEnv
      | _ -> failwith "eval Call: not a function"
      end

(* Evaluate in empty environment: program must have no free variables: *)

let run e = eval e []

(* Examples in abstract syntax *)

let ex1 = Letfun("f1", "x", Prim("+", Var "x", CstI 1),
                 Call(Var "f1", CstI 12))

(* Factorial *)

let ex2 = Let("n", CstI 5,
              Letfun("fac", "x",
                 If(Prim("=", Var "x", CstI 0),
                    CstI 1,
                    Prim("*", Var "x",
                              Call(Var "fac",
                                   Prim("-", Var "x", CstI 1)))),
                 Call(Var "fac", Var "n")))

(* let fac10 = eval ex2 [("n", Int 10)];; *)

let ex3 = Letfun("tw", "g",
           Letfun("app", "x", Call(Var "g", Call(Var "g", Var "x")),
                  Var "app"),
           Letfun("mul3", "y", Prim("*", CstI 3, Var "y"),
                  Call(Call(Var "tw", Var "mul3"), CstI 11)))

let ex4 = Letfun("tw", "g",
           Letfun("app", "x", Call(Var "g", Call(Var "g", Var "x")),
                  Var "app"),
           Letfun("mul3", "y", Prim("*", CstI 3, Var "y"),
                  Call(Var "tw", Var "mul3")))

let ex5 = Let("n",Tuple(CstI 1,CstI 2),Letfun("fa","x",Prim("proj",Var "x",CstI 1),Call(Var "fa",Var "n")))

let ex6 = Let("x",Tuple(CstI 1,Tuple(CstI 2, CstI 3)),Var "x");;

let ex8 = Let("x",List(CstI 1,List(CstI 2, Empty)), Iop("head",Var "x"));;
let ex9 = Let("x",List(CstI 1,List(CstI 2, Empty)), Iop("tail",Var "x"));;
let ex7 = Let("x",List(CstI 1,List(CstI 2, Empty)), Prim("cons", CstI 1, Var "x"));;