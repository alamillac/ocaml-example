(*
module type ModuleTypeName = sig
  specifications
end

- specifications include val, type, exceptions, module type

module ModuleName : Mt = struct
  definitions
end

module ModuleName = (struct
  definitions
end : Mt)


if you give a module a type...
  module Mod : Sig = struct ... end

Then type checker ensures...
- Signature matching: everything specified in Sig must
  be defined in Mod with the right types
- Encapsulation: only what is specified in Sig can be
  accessed outside Mod. The module is sealed.

*)

module type Fact = sig
  (** [fact n] is [n] factorial. *)
  val fact : int -> int
end

module RecursiveFact : Fact = struct
  let rec fact n =
    if n = 0 then 1 else
      n * fact (n - 1)
end

module TailRecursiveFact : Fact = struct
  let rec fact_aux n acc =
    if n = 0 then acc else
      fact_aux (n - 1) (n * acc)

  let fact n =
    fact_aux n 1
end

(* let x = TailRecursiveFact.fact_aux 10 *)

(* module NotFact : Fact = struct *)
(*   let inc x = x + 1 *)
(* end *)
