let rec swap xs i j =
  let tmp = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- tmp
in
let rec next_left xs pivot i =
  (* pivot <= xs.(i) *)
  if pivot <= xs.(i) then i else next_left xs pivot (i + 1)
in
let rec next_right xs pivot i =
  (* pivot >= xs.(i) *)
  if xs.(i) < pivot then i else next_right xs pivot (i - 1)
in
let rec partition xs l r =
  let l = next_left l in
  let r = next_right r in
  if l >= r
  then l, r
  else (
    let xs = swap xs l r in
    partition xs (l + 1) (r - 1))
in
let rec go xs left right =
  if right <= left then xs
  else (
    let l = left + right in
    let pivot = xs.(l / 2) in
      let (l, r) = partition xs left right in
      let xs = go xs left (l - 1) in
      go xs (r + 1) right)
in
let rec quick_sort xs len =
  go xs 0 (len - 1)
in
let a = Array.make 5 0 in
a.(0) <- 4;
a.(1) <- 8;
a.(2) <- 1;
a.(3) <- 1;
a.(4) <- 5;
let sorted = quick_sort a 5 in
print_int (sorted.(0))
