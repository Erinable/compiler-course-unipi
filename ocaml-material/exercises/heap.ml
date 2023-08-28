type 'a t = Leaf | Node of 'a t * 'a t * int * 'a

exception MyException of string

let empty = Leaf

let singleton a = Node(Leaf,Leaf,1,a)

let rec merge h1 h2 = match h1,h2 with
  | Leaf,_ -> h2
  | _,Leaf -> h1
  | Node(lh1,rh1,_,kh1),Node(_,_,_,kh2) ->
    if kh1 > kh2 then
       merge h2 h1
    else
      let rh = merge rh1 h2 in
      match lh1,rh with
      | Leaf,_ -> Node(rh,lh1,1,kh1)
      | Node(_,_,s1,_),Node(_,_,s2,_) -> if s1 < s2 then Node(rh,lh1,s1+1,kh1) else Node(lh1,rh,s2+1,kh1)
      | _,Leaf -> raise (MyException "This is an exception")

let insert k h = merge h (singleton k)

let get_min = function
  | Node(_,_,_,a) -> a
  | _ -> None

let delete_min = function
  | Node(l,r,_,_) -> merge l r
  | _ -> empty

let rec generate_heap = function
  | l::t -> insert l (generate_heap t)
  | [] -> empty