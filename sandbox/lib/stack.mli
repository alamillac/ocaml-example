(** The type of a stack whose elements are type 'a *)
type 'a t

(** The empty stack *)
val empty : 'a t

(** [push x s] is th stack [s] with [x] pushed on the top *)
val push : 'a -> 'a t -> 'a t

(** [peek s] is the top element of [s].
    Raises Failure if [s] is empty. *)
val peek : 'a t -> 'a

(** [pop s] pops and discards the top element of [s].
    Raises Failure if [s] is empty. *)
val pop : 'a t -> 'a t
