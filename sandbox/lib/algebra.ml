module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val string : t -> string
end

module IntRingRep = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let string = string_of_int
end

module IntRing : Ring = IntRingRep

module FloatRingRep = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let string = string_of_float
end

module FloatRing : Ring = FloatRingRep

module type Field = sig
  include Ring (* Include all the declarations that are in Ring *)
  val ( / ) : t -> t -> t
end

module IntFieldRep = struct
  include IntRingRep (* Include all the definitions that are in IntRingRep *)
  let ( / ) = Stdlib.( / )
end

module IntField : Field = IntFieldRep

let x = IntRing.(zero + zero |> string)
let y = FloatRing.(zero + zero |> string)
let z = IntField.(zero + one / one |> string)
