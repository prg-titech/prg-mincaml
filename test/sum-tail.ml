let rec sum acc x =
  if x < 1 then acc else
  let acc = acc + x in
  let x = x - 1 in
  sum acc x in
print_int (sum 0 10)
