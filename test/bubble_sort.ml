let%tj rec swap xs idx max =
  if max < idx + 1
  then xs
  else (
    let l = xs.(idx) in
    let r = xs.(idx + 1) in
    if l < r + 1
    then swap xs (idx + 1) max
    else (
      xs.(idx) <- r;
      xs.(idx + 1) <- l;
      swap xs (idx + 1) max))
in
let rec bubble_sort xs idx len =
  if len - 1 < idx
  then xs
  else (
    let xs = swap xs 0 (len - idx) in
    bubble_sort xs (idx + 1) len)
in
let rec make_arr n arr =
  if n < 1 then arr else
    (arr.(n-1) <- rand_int 2048; make_arr (n-1) arr)
in
let a = Array.make 10 0 in
a.(0) <- 3; a.(1) <- 1; a.(2) <- 2;
a.(3) <- 10; a.(4) <- (-1); a.(5) <- 8;
let b = bubble_sort a 0 10 in
print_int (a.(0))
