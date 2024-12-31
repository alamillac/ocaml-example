type intlist =
  | Nil
  | Cons of int * intlist

let rec length_intlist = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length_intlist t

let ex_list = Cons (1, Cons (2, Nil))
let ex_length = length_intlist ex_list

type 'a mylist =
  | []
  | (::) of 'a * 'a mylist

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t

let ex2_list = 1 :: 2 :: []
let ex2_length = length ex2_list
