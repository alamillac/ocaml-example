(* Compilation unit = myfile.ml + myfile.mli
   if myfile.ml has contents DM
   and myfile.mli has contents DS
   then OCaml compiler behaves essentially as though:

   module Myfile [: sig DS end] = struct
     DM
   end
*)

(** Stacks are represented by lists. The top of the
    stack is the head of the list. The bottom of the
    stack is the last element of the list. *)
type 'a t = 'a list

let empty = []

let push x s =
  x :: s

let peek = function
  | [] -> failwith "Empty"
  | x :: _ -> x

let pop = function
  | [] -> failwith "Empty"
  | _ :: s -> s
