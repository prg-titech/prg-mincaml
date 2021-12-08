let%mj rec sum i n =
  if i <= 1 then n else
    sum (i-1) (n+i)
in
let%mj rec tak x y z =
  if x <= y then sum 1000 0 else
    tak (tak (x-1) y z) (tak(y-1) z x) (tak(z-1) x y)
in
print_int (tak 8 4 2)
