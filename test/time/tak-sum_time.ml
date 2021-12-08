let rec sum i n =
  if i <= 1 then n else
    sum (i-1) (n+i)
in
let rec tak x y z =
  if x <= y then sum 1000 0 else
    tak (tak (x-1) y z) (tak(y-1) z x) (tak(z-1) x y)
in
let s = get_current_micros () in
let _ = tak 8 4 2 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
