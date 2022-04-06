(* let rec make_arr n arr = *)
(*   if n < 1 then arr else *)
(*     (arr.(n-1) <- rand_int 2048; make_arr (n-1) arr) *)
(* in *)
let rec swap xs i n =
  if n-1 < i then xs
  else
    let l = xs.(i) in
    let r = xs.(i+1) in
    if l > r then (
      xs.(i) <- r; xs.(i+1) <- l;
      swap xs (i+1) n
    ) else swap xs (i+1) n
in
let rec bubble_sort xs i n =
  if n-1  < i then xs
  else
    let xs = swap xs 0 (n-i-1) in
    bubble_sort xs (i+1) n
in
let a = Array.make 1000 1 in
a.(0) <- 3; a.(1) <- 1; a.(2) <- 2;
a.(3) <- 10; a.(4) <- 1;
let b = bubble_sort a 0 1000 in
print_int (b.(999))
