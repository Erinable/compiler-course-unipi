let rec swap_adjacent1 = function
  | first::second::tl -> second::first::(swap_adjacent1 tl)
  | l -> l
let explode_string s = List.init (String.length s) (String.get s)
let implode_char_list l = String.of_seq (List.to_seq l)
let swap_adjacent l = implode_char_list (swap_adjacent1 (explode_string l))

let rec c n k =
  match n=k || k=1 with
  | true -> 1
  | false -> c (n-1) k + c (n-1) (k -1)
