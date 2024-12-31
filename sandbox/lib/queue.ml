module ListQueueImpl = struct
  type 'a queue = 'a list

  let empty = []

  let enqueue x q =
    q @ [x] (* linear time :( *)

  let peek = function
    | [] -> None
    | x :: _ -> Some x

  let dequeue = function
    | [] -> None
    | _ :: q -> Some q
end

module type Queue = sig
  (** ['a queue] is the representation type of queues. *)
  type 'a queue

  (** [empty] is the empty queue *)
  val empty : 'a queue

  (** [enqueue x q] is [q] with [x] at the end of the queue. *)
  val enqueue : 'a -> 'a queue -> 'a queue

  (** [peek q] is [Some x], where [x] is the front element of
      [q], or [None] if [q] is empty. *)
  val peek : 'a queue -> 'a option

  (** [dequeue q] is [Some 'q], where [q'] is all but the
      front element of [q], or [None] if [q] if empty. *)
  val dequeue : 'a queue -> 'a queue option
end

module TwoListQueueImpl = struct
  (* [{front = [a; b]; back = [e; d; c]}]
     represents the queue a,b,c,d,e.
     If [front] is empty then [back] must also be empty,
     to guarantee that the first element of the queue
     is always the head of [front].
  *)
  type 'a queue = {
    front : 'a list;
    back : 'a list;
  }

  let empty = {
    front = [];
    back = [];
  }

  let peek = function
    | {front = []; _} -> None
    | {front = x :: _; _} -> Some x

  let enqueue x = function
    | {front = []; _} -> {front = [x]; back = []}
    | q -> {q with back = x :: q.back} (* constant time !!! *)

  let dequeue = function
    | {front = []; _} -> None
    | {front = _ :: []; back} ->
      Some {front = List.rev back; back = []}
    | {front = _ :: t; back} -> Some {front = t; back}
      (* constant time except when front gets exhausted *)
end

module ListQueue : Queue = ListQueueImpl
module TwoListQueue : Queue = TwoListQueueImpl

let ( |> ) x f =
  f x

(* Option.map in stdlib *)
let ( >>| ) opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)

(* Option.bind in stdlib *)
let ( >>= ) opt f =
  match opt with
  | None -> None
  | Some x -> f x

let q : int ListQueue.queue option =
  ListQueue.(empty |> enqueue 42 |> dequeue >>| enqueue 43 >>= dequeue)

let w : int ListQueue.queue option =
  let open ListQueue in
  empty
  |> enqueue 42
  |> dequeue
  >>| enqueue 43
  >>= dequeue

let x : int ListQueue.queue option =
  ListQueue.(empty |> enqueue 42 |> dequeue)

let z : int ListQueue.queue option = Option.map (ListQueue.enqueue 42) x
let d : int ListQueue.queue option = Option.bind z (ListQueue.dequeue)
