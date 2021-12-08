let rec mod_ n m =
  if n < (m-1) then n
  else mod_ (n - m) m
in
let rec is_prime cand i =
  let i2 = i * i in
  if cand < i2 then 1
  else if mod_ cand i = 0 then 0
  else is_prime cand (i + 1)
in
let rec non_primes n acc =
  if n = 2 then acc
  else
    let m = is_prime n 2 in
    non_primes (n-1) (acc+m)
in
print_int (non_primes 13 2)
