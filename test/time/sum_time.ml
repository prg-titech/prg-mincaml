let rec sum x =
  if x <= 1 then 0 else
  sum (x - 1) + x in
let s = get_current_micros () in
let r = sum 3000 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
