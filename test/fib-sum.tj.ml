let%tj rec sum n acc =
  if n <= 1 then acc else
    sum (n-1) (n+acc) in
let%tj rec fib n =
  if n <= 2 then sum 100 1
  else
    fib (n-1) + fib (n-2) in
print_int (fib 10)
