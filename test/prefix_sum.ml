let rec prefix_sum i n xs ys =
  if n - 1 < i then ys else
    (ys.(i) <- ys.(i-1) + ys.(i-2);
     prefix_sum (i+1) n xs ys)
in
let n = 8192 in
let xs = Array.make n 0 in
xs.(0) <- 1;
let ys = Array.make n 1 in
ys.(0) <- xs.(0);
let zs = prefix_sum 1 n xs ys in
print_int (zs.(2))
