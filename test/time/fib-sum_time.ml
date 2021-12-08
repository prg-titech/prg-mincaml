let rec sum n acc =
  if n <= 1 then acc else
    sum (n-1) (n+acc) in
let rec fib n =
  if n <= 1 then sum 1000 0 else
    fib (n-1) + fib (n-2) in
let rec loop i =
  if i = 0 then () else
    let s = get_current_micros () in
    let r = fib 20 in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 150
