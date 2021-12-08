let%tj rec f x = (-x) in
let%mj rec g x =
  if x < 0 then 0 else x + g (x-1) in
print_int (f (g 3))
