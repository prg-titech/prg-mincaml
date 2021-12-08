let%tj rec fib n a b =
  if n = 0 then a
  else if n = 1 then b
  else  fib (n-1) b (a+b)
in
let%tj rec sum i n =
  if i <= 1 then n else
    let m = fib 10 0 1 in
    sum (i-1) (n+m) in
print_int (sum 1000 0)
