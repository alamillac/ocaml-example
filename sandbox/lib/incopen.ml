(*
  include vs. open
  open M:
  - imports definitions from M
  - makes them available for local consumption
  - doesn't export them to outside world

  include M:
  - imports definitions from M
  - makes them available for local consumption
  - does export them to outside world
*)

module M = struct
  let x = 0
end

module N = struct
  include M (* include all the definitions of module M *)
  let y = x + 1
end

module O = struct
  open M (* open the definitions of module M. Making its names available for use
            internally but we don't export them again *)
  let y = x + 1
end

(*
  module M : sig val x : int end
  module N : sig val x : int val y : int end
  module O : sig val y : int end
*)
