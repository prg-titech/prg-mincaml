let%mj rec fib n =
  if n <= 1 then 1 else
    fib (n-1) +
    fib (n-2) in
let rec sum i =
  if i <= 1 then 1 else
    let m = fib 10 in
    m + sum (i-1) in
print_int (sum 100)
