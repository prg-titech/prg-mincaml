let rec square i x =
  if i <= 1 then x else
    x + square (i-1) x
in
print_int (square 2500 2500)
