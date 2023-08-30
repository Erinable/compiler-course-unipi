type expr = LET of string * expr | Prim of string * expr * expr | Int of int | Var of string | Float of float

type 'v env = (string * 'v) list


type num = INT of int | FLOAT of float
type value = string * num

let calc_env : (string, num) Hashtbl.t = Hashtbl.create 10

let calc_fuc (f:float->float->float) (v1:num) (v2:num):num = match v1, v2 with
  | INT a, INT b -> INT (int_of_float (f (float_of_int a) (float_of_int b)))
  | INT a, FLOAT b -> FLOAT(f (float_of_int a) b)
  | FLOAT a, INT b -> FLOAT(f a (float_of_int b))
  | FLOAT a, FLOAT b -> FLOAT(f a b)

let sin1 a b = sin a
let cos1 a b = cos a
let log1 a b = log a
let rec eval (e:expr):value = match e with
| Int i -> ("-",INT i)
| Float f -> ("-",FLOAT f)
| Var v -> (v,Hashtbl.find calc_env v)
| LET(id,e) -> let v = eval e in
    begin
    match snd v with
    | a -> (Hashtbl.add calc_env id a);(id,a)
    end
| Prim(op,e1,e2) ->
    let v1 = eval e1 in let v2 = eval e2 in
    match op with
    | "+" -> ("-",calc_fuc ( +. ) (snd v1) (snd v2))
    | "/" -> ("-",calc_fuc ( /. ) (snd v1) (snd v2))
    | "*" -> ("-",calc_fuc ( *. ) (snd v1) (snd v2))
    | "-" -> ("-",calc_fuc ( -. ) (snd v1) (snd v2))
    | "sin" -> ("-",calc_fuc (sin1) (snd v1) (snd v2))
    | "cos" -> ("-",calc_fuc (cos1) (snd v1) (snd v2))
    | "log" -> ("-",calc_fuc (log1) (snd v1) (snd v2))
| _ -> failwith ("eval error")


