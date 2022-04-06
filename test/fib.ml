(* let rec fib n = *)
(*   if 1 < n then *)
(*       fib (n-1) + fib(n-2) *)
(*   else 1 in *)
(* print_int (fib 25) *)
let rec fib n =
  if n <= 1 then 1
  else fib(n-1) + fib(n-2)
in print_int (fib 25)
