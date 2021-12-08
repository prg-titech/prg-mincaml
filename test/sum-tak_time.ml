let rec tak x y z =
  if x <= y then z else
    tak (tak (x-1) y z) (tak(y-1) z x) (tak(z-1) x y)
in
let rec sum i =
  let m = tak 12 6 4 in
  if i <= 1 then m else
    m + sum (i-1)
in
let s = get_current_micros () in
let _ =  (sum 100) in
let e = get_current_micros () in
print_int (e-s); print_newline ()
