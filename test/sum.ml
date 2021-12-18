let rec sum x =
  if x < 2 then 1 else
  x + sum (x - 1) in
print_int (sum 3000)
