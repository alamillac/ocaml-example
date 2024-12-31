(*
- Everything name specified in Sig must be defined in Mod
- The types in Mod must be the same as those in Sig or more general
*)

module type IntFun = sig
  val f : int -> int
end

module SuccFun : IntFun = struct
  let f x = x + 1
end

module IdFun : IntFun = struct
  let f x = x
end

let a = IdFun.f 12
(* let b = IdFun.f "Not valid" *)
