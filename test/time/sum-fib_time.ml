let rec fib n =
  if n <= 1 then 1 else
    fib (n-1) + fib (n-2) in
let rec sum i n =
  if i <= 1 then n else
    let m = fib 10 in
    sum (i-1) (m + n) in
let rec loop i =
  if i < 0 then () else
    let s = get_current_micros () in
    let r = sum 1000 0 in
    let e = get_current_micros () in
    print_int r; print_newline ();
    print_int (e-s); print_newline ();
    loop (i-1) in
loop 150
