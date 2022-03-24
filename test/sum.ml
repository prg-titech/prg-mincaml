let rec sum x =
  if x < 1 then x else
  x + sum (x - 1) in
print_int (sum 10)
