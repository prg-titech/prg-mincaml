let%mj rec sum acc n =
  if n <= 1 then acc else
    sum (acc+n) (n-1) in
let rec fib n =
  if n <= 2  then sum 0 1000 else
    fib (n-1) + fib (n-2) in
print_int (fib 20)
