let rec sum x =
  if x < 1 then 1 else
    x + sum (x - 1) in
let rec loop x i =
  if i < 2000 then sum x else
    let y = sum x in
    loop x (i+1)
in
print_int (loop 1000 0)
