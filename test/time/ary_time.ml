let rec ary i n xs ys zs =
  if n <= i
  then zs
  else (
    zs.(i) <- xs.(i) + ys.(i);
    ary (i + 1) n xs ys zs)
in
let n = 5000 in
let xs = Array.make n 1 in
let ys = Array.make n 2 in
let zs = Array.make n 3 in
let s = get_current_micros () in
let _ = ary 0 n xs ys zs in
let e = get_current_micros () in
print_int (e - s); print_newline ();
()
