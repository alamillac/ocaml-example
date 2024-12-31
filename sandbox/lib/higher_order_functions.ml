let double x = 2 * x
let square x = x * x

let quad x = 4 * x
let quad' x = double (double x)
let quad'' x = x |> double |> double

let fourth x = x * x * x * x
let fourth' x = square (square x)
let fourth'' x = x |> square |> square

let twice f x = f (f x)
let twice' f x = x |> f |> f

let quad''' x = twice double x
let quad'''' = twice double (* partial application *)

let fourth''' x = twice square x
let fourth'''' = twice square (* partial application *)
