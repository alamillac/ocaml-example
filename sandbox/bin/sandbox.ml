(* Use utop to read this file:
   utop
   #use "sandbox.ml";;
   #trace sum;;
   #untrace sum;;
*)

type person = {
    (* first_name : string; *)
    (* surname : string; *)
    age : int
}

(*
type alien = {
    planet : string;
    age : int
}
*)

let is_teenager (person: person) =
    match person with
    | { age = x; _ } -> 13 <= x && x <= 19

let gerard = {
     age = 76
}

let () = print_endline ("is_teenager: " ^ string_of_bool (is_teenager gerard))

(* Patter matching *)
let y =
  match 42 with
  | fooo -> fooo
let () = print_endline ("y: " ^ string_of_int y)

let b =
  match ["one"; "two"] with
  | [] -> "empty"
  | h :: _ -> h
let () = print_endline ("b: " ^ b)

(* Patter matching with tuples *)
let fst3 t =
  match t with
  | (a, _, _) -> a
let () = print_endline ("fst3: " ^ fst3 ("one", "two", "three"))
let () = print_endline ("fst: " ^ fst ("one", "two"))

let snd3 = function
  | (_, b, _) -> b
let () = print_endline ("snd3: " ^ snd3 ("one", "two", "three"))
let () = print_endline ("snd: " ^ snd ("one", "two"))

(* Patter matching with records *)

type player = {
  name: string;
  year: int
}

let rbg = {
  name = "Ruth Bader";
  year = 1954;
}

let name_with_year s =
  match s with
  | {name; year} -> name ^ " " ^ string_of_int year
let () = print_endline ("name_with_year: " ^ name_with_year rbg)

(* Patter matching with list *)
let empty lst =
    match lst with
    | [] -> true
    | _ -> false

let () = print_endline ("empty: " ^ string_of_bool (empty [1; 2; 3]))

let empty2 = function
    | [] -> true
    | _ -> false

let () = print_endline ("empty2: " ^ string_of_bool (empty2 [1; 2; 3]))

let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t
let () = print_endline ("sum: " ^ string_of_int (sum [1;2;3]))

(* Example usage:
   append [1;2;3] [4;5;6] is [1;2;3;4;5;6] *)
let rec append lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h :: t -> h :: append t lst2

(* There is an operator @ that append two list *)
let () = print_endline ("append: " ^ string_of_bool ((append [1;2] [3;4]) = [1;2] @ [3;4]))

(* List operators "cons" :: vs "append" @ *)
(* cons add an element onto the head of a list. Ex: 1 :: [2;3] *)
(* append combine two lists. Ex [1;2] @ [3] *)

let fst' = function
  | (x, _) -> x

let snd' tpl = let (_, y) = tpl in y

let center_c1 = Sandbox.Variants.center Sandbox.Variants.c1
let center_r1 = Sandbox.Variants.center Sandbox.Variants.r1
let () = Printf.printf "Center c1: (%f, %f)\n" (fst' center_c1) (snd' center_c1)
let () = Printf.printf "Center r1: (%f, %f)\n" (fst center_r1) (snd center_r1)

module DayMap = Map.Make(Sandbox.Daymap.DayKey)

let m =
  let open DayMap in
  empty
  |> add Mon "Monday"
  |> add Tue "Tuesday"

let () =
  let open DayMap in
  Printf.printf "Mon in m: %s\n" (string_of_bool (mem Mon m))

let () =
  let open DayMap in
  Printf.printf "Sun in m: %s\n" (string_of_bool (mem Sun m))

let () =
  let open DayMap in
  Printf.printf "Mon: %s\n" (find Mon m)
