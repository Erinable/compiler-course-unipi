let rec sum_digit x:int =
  let n = (x mod 10) in
  match x < 10 with
  | true -> x
  | false -> n + sum_digit (x / 10)

let rec super_digit x:int =
  match x < 10 with
  | true -> x
  | false -> super_digit (sum_digit x)

let rec list_replication l (n:int) : 'a =
  match n > 1 with
  | true -> list_replication (l @ l) (n -1)
  | false -> l

let rec element_replication a n =
  match n > 1 with
  | true -> a :: element_replication a (n-1)
  | false -> [a]

let rec list_replication1 l (n:int) : 'a =
  match l,n > 1 with
  | hd::tl,true -> element_replication hd n @ list_replication1 tl n
  | _,_ -> l

module IntSet = Set.Make(Int);;
let l_s = IntSet.empty
let rec is_funciton l_s = function
  | [] -> true
  | (x,_)::tl ->
    let s = IntSet.find_opt x l_s in
    match s with
    | Some x -> false
    | None -> let l_s = IntSet.add x l_s in is_funciton l_s tl

let rec mingle_string l1 l2 =
  let n = String.length l1 in
  if n > 1 then
    Char.escaped l1.[0] ^ Char.escaped l2.[0] ^ mingle_string (String.sub l1 1 (n-1)) (String.sub l2 1 (n-1))
  else
    l1 ^ l2

let rec s (x:float) (n:int) : float = match n with
  | 0 -> 1.0
  | _ ->
    let rec f (x:float) (n:int) : float = match n with
    | 0 -> 1.0
    | _ -> x /. float_of_int n *. f x (n-1) in
    s x (n-1) +. f x n

let g x = s x 9

let () = print_float (g 1.0);