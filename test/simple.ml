let%mj rec f x =
  if 0 < x then 2048 else 1024
in
let%tj rec g y =
  let z = f y in
  if 1 < y then g (y-1) else y + z
in print_int (g 10)
