let rec prefix_sum i n xs ys =
  if i < n then
    (ys.(i) <- ys.(i-1) + i;
     prefix_sum (i+1) n xs ys)
  else
    ys in
let n = 1000 in
let xs = Array.make n 0 in
xs.(0) <- 1;
let ys = Array.make n 1 in
ys.(0) <- xs.(0);
let s = get_current_micros () in
let zs = prefix_sum 1 n xs ys in
let e = get_current_micros () in
print_int (e-s); print_newline ()
