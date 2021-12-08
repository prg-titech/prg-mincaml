let rec square i x acc =
  if i <= 1 then acc else
    square (i-1) x (x+acc)
in print_int (square 10000 10000 0)
