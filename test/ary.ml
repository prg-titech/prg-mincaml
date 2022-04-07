let rec ary i n xs ys zs =
  if n-1 < i then zs
  else (
    zs.(i) <- xs.(i) + ys.(i);
    ary (i + 1) n xs ys zs)
in
let n = 8196 in
let xs = Array.make n 1 in
let ys = Array.make n 2 in
let zs = Array.make n 0 in
let arr = ary 0 n xs ys zs in
print_int (arr.(0))
