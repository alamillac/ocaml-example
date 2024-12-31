module type Set = sig
  (** ['a t] it the type of a set whose elements have type ['a]. *)
  type 'a t

  (** [empty] is the empty set. *)
  val empty : 'a t

  (** [size s] is the number of elements in [s].
      [size empty] is [0]. *)
  val size : 'a t -> int

  (** [add x s] is a set containing all the elements of [s]
      as well as element [x]. *)
  val add : 'a -> 'a t -> 'a t

  (** [mem x s] is [true] if [x] is an element of [s]. *)
  val mem : 'a -> 'a t -> bool

  (** [union s1 s2] is the set containing both the elements
      of [s1] and the elements of [s2]. *)
  val union : 'a t -> 'a t -> 'a t

  (** [string f s] is a representation of [s] as a string, using
      [f] to represent elements as strings. *)
  val string : ('a -> string) -> 'a t -> string
end

let interior string_of_elt h t =
  t
  |> List.map string_of_elt
  |> List.fold_left (fun acc elt -> acc ^ ", " ^ elt) (string_of_elt h)

let string_of_list string_of_elt = function
  | [] -> "{}"
  | h :: t -> "{" ^ interior string_of_elt h t ^ "}"

(** [dedup lst] is [lst] but with duplicates removed. It also
    sorts the output list. *)
let dedup lst =
  lst |> List.sort_uniq Stdlib.compare

module ListSetNoDups : Set = struct
  (** AF (Abstraction function): The list [a1; ...; an] represents the set {a1, ..., an}.
      The empty list [[]] represents the empty set.
      RI (Representation invariant): The list must not contain duplicates. *)
  type 'a t = 'a list

  let empty = []

  let size = List.length

  let mem = List.mem

  let add x s =
    if List.mem x s then s else x :: s

  let union s1 s2 =
    s1 @ s2 |> List.sort_uniq Stdlib.compare

  let string f s =
    string_of_list f s
end

module ListSetDups : Set = struct
  (** AF: The list [a1; ...; an] represents the set {b1, ..., bm}
      where [b1; ...; bm] is the same list as [a1; ...; an]
      but with any duplicates removed.
      The empty list [[]] represents the empty set.
      RI: None: the list may contain duplicates. *)
  type 'a t = 'a list

  let empty = []

  let mem = List.mem

  (* let rec size = function (* Quadratic time *) *)
  (*   | [] -> 0 *)
  (*   | h :: t -> (if mem h t then 0 else 1) + size t *)

  let size s =
    s |> dedup |> List.length

  let add = List.cons (*  ::  *)

  let union = List.append (* @ *)

  let string f s =
    s |> dedup |> string_of_list f
end

let rep = ListSetDups.(empty |> add 42 |> add 43 |> string string_of_int)
