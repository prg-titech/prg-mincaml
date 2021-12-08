let rec fact x =
  if x <= 1.0 then 1.0 else
    x *. fact (x -. 1.0) in
let s = get_current_micros () in
let r = fact 3000.0 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
