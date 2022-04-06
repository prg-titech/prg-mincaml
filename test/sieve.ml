let rec loop1 flags k i n =
  if n-1 < k then flags
  else
    (flags.(k) <- 1;
    loop1 flags (k+i) i n)
in
let rec sieve flags i n =
  if n - 1 < i then flags
  else
    let x = flags.(i) in
    if x < 2 then
      sieve flags (i+1) n
    else
      let flags = loop1 flags (i+i) i n in
      sieve flags (i+1) n
in
let n = 131136 in
let flags = Array.make n 2 in
flags.(0) <- 1; flags.(1) <- 1; flags.(2) <- 1;
let _ = sieve flags 3 n in
print_int (flags.(20))
