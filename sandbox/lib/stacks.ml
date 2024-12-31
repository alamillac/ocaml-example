module type ListStackSig = sig
  (** [empty] is the empty stack *)
  val empty : 'a list

  (** [push x s] is [s] with [x] pushed on top *)
  val push : 'a -> 'a list -> 'a list

  (** [peek s] is the top element of [s].
      Raises [Failure] if [s] is empty. *)
  val peek : 'a list -> 'a

  (** [pop s] is all but the top element of [s].
      Raises [Failure] if [s] is empty. *)
  val pop : 'a list -> 'a list
end

module type StackSig = sig
  (** ['a t] -> ['a stack] is the representation type for stacks *)
  type 'a t (* abstract type. Signature declares only that it exist,
               but does not define what it is *)

  (** [empty] is the empty stack *)
  val empty : 'a t

  (** [push x s] is [s] with [x] pushed on top *)
  val push : 'a -> 'a t -> 'a t

  (** [peek s] is the top element of [s].
      Raises [Failure] if [s] is empty. *)
  val peek : 'a t -> 'a

  (** [pop s] is all but the top element of [s].
      Raises [Failure] if [s] is empty. *)
  val pop : 'a t -> 'a t
end

module MyStack : StackSig = struct
  type 'a t =
    | Empty
    | Entry of 'a * 'a t

  let empty = Empty

  let push x s =
    Entry (x, s)

  let peek = function
    | Empty -> failwith "Empty"
    | Entry (x, _) -> x

  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_, s) -> s
end

module ListStackImpl = struct
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
end

module ListStack : StackSig = ListStackImpl

let s1 : int ListStack.t = ListStack.(empty |> push 42)
(* let s2 : int ListStack.t = [42] (* This generate a type error *) *)
let s2 : int ListStackImpl.t = [42]

let s = ListStack.empty
let s' = ListStack.push 1 s
let x = ListStack.peek s'

let a = ListStack.(peek (push 42 empty))
let b = ListStack.(empty |> push 42 |> peek)

let c =
  let open ListStack in
  empty |> push 42 |> peek

open ListStack (* Discouraged because it can shadow other values, like empty. Its useful on testing files where we only test a single module *)
let v = empty |> push 42 |> peek

open MyStack
let u = empty
