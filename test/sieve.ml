let rec loop1 flags k i =
  if k <= 8195 then
    (flags.(k) <- 1;
     loop1 flags (k+i) i)
  else flags in
let rec sieve flags i =
  if i <= 8195 then
    let x = flags.(i) in
    if x > 1 then
      let flags = loop1 flags (i+i) i in
      sieve flags (i+1)
    else
      sieve flags (i+1)
  else
    flags in
let flags = Array.make 8196 2 in
flags.(0) <- 1; flags.(1) <- 1;
let _ = sieve flags 2 in
print_int (flags.(14));
()
