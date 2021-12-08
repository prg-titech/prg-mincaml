let%tj rec sum x =
  if x < 1 then 1 else
  sum (x - 1) + x in
print_int (sum 10000)
