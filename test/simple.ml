let%mj rec f x =
  if x < 0 then 1024 else 2048
in
let%tj rec g y =
  let z = f y in
  if y < 1 then y + z else
    g (y-1)
in print_int (g 10)
