let rec loop1 flags k i =
  if k <= 8196 then
    (flags.(k) <- (-1);
     loop1 flags (k+i) i)
  else flags in
let rec sieve flags i =
  if i <= 8196 then
    if flags.(i) = 1 then
      let _ = loop1 flags (i+1) i in
      sieve flags (i+1)
    else
      sieve flags (i+1)
  else
    flags in
let s = get_current_micros () in
let flags = Array.make 8196 1 in
let _ = sieve flags 2 in
let e = get_current_micros () in
print_int (e - s); print_newline ()
