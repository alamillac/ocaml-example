let rec evens = function
  | [] -> []
  | h :: t -> begin
      if h mod 2 = 0
      then h :: evens t
      else evens t
    end

let rec odds = function
  | [] -> []
  | h :: t -> begin
      if h mod 2 = 1
      then h :: odds t
      else odds t
    end

let rec filter' p = function
  | [] -> []
  | h :: t -> begin
      if p h
      then h :: filter' p t
      else filter' p t
    end

let even x =
  x mod 2 = 0

let odd x =
  x mod 2 = 1

let evens' lst = filter' even lst
let odds' lst = filter' odd lst

(* Tail recursive version *)
let rec filter_aux p acc = function
  | [] -> List.rev acc
  | h :: t -> filter_aux p (if p h then h :: acc else acc) t

let filter p lst = filter_aux p [] lst
let evens_list = filter even [1;2;3;4]
