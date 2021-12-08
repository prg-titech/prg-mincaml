let rec fib n a b =
  if n = 0 then a
  else if n = 1 then b
  else fib (n-1) b (a + b) in
let s = get_current_micros () in
let _ = (fib 20000 0 1) in
let e = get_current_micros () in
print_int (e-s); print_newline ()
