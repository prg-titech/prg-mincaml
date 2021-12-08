let%tj rec fib n =
  if n < 2 then 1 else
    fib (n-1) + fib (n-2) in
let%tj rec sum i n =
  if i < 0 then n else
    sum (i-1) (n + (fib i)) in
print_int (sum 30 0)
