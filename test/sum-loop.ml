let rec sum x =
  if x < 2 then 1 else
    x + sum (x - 1) in
let rec loop n x =
  if n < 1 then sum x
  else let _ = sum x in loop (n - 1) x
in
print_int (loop 100 10)
