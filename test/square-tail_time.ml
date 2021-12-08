let rec square i x acc =
  if i <= 1 then acc else
    square (i-1) x (x+acc) in
let s = get_current_micros () in
let _ = square 10000 10000 0 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
