let%mj rec fib n =
  if n <= 1 then 1 else
    fib (n-1) + fib (n-2) in
let%tj rec sum i n =
  if i <= 1 then n else
    let m = fib 10 in
    sum (i-1) (n+m) in
print_int (sum 1000 0)
