let rec fib n a b =
  if n = 0 then a
  else if n = 1 then b
  else fib (n-1) b (a + b)
in print_int (fib 20000 0 1)
