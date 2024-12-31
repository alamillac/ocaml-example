(* Exceptions are extensible variants. type exn *)

exception ABadThing
exception OhNo of string

let () = raise ABadThing
let x : int = raise (OhNo "this is bad")

let () = raise (Failure "Argument out of range")
let () = failwith "Argument out of range"

let () = raise (Invalid_argument "arg")
let () = invalid_arg "arg"

let safe_div x y =
  try x / y with
  | Division_by_zero -> 0
  | ABadThing -> 0
  | OhNo _error_msg -> 0
