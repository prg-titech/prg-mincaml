let rec square i x =
  if i < 0 then 0 else
    x + square (i-1) x in
let s = get_current_micros () in
let _ = square 3000 3000 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
