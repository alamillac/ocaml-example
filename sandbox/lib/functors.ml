module type X = sig
  val x : int
end

module A = struct
  let x = 0
end

module B : X = struct
  let x = 0
end

module IncX = functor (M : X) -> struct
  let x = M.x + 1
end

module IncX' (M : X) = struct (* syntactic sugar for functors *)
  let x = M.x + 1
end

module C = IncX(A)
module D = IncX'(B) (* D.x => int = 1 *)
module E = IncX'(D) (* E.x => int = 2 *)

(*
  Syntax:

  (* Like syntax for multi-argument functions, except type annotations on inputs are required *)
  module F (M1 : S1) ... (Mn : Sn) = struct
    ...
  end


  (* Like desugaring of multi-argument functions, but with functor instead of fun *)
  module F =
    functor (M1 : S1)
    -> ...
    -> functor (Mn : Sn)
    -> struct
    ...
  end
*)
