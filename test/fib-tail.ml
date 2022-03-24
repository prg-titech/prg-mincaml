let rec fib a b n =
  if n < 1 then a
  else if n < 2 then b
  else fib b (a + b) (n-1)
in print_int (fib 0 1 250)
