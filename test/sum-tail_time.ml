let rec sum acc x =
  if x <= 0 then acc else
  sum (acc + x) (x - 1) in
let rec loop i =
  if i < 1 then ()
  else
    let s = get_current_micros () in
    let _ = sum 0 10000 in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 1
