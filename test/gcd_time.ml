let rec gcd m n =
  if m <= 0 then n else
  if m <= n then gcd m (n - m) else
  gcd n (m - n) in
let rec loop i =
  if i = 0 then () else
    let s = get_current_micros () in
    let _ = gcd 216 3375000  in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 100
