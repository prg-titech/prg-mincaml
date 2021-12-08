let rec mod_ n m =
  if n < (m -. 1.0) then n
  else mod_ (n -. m) m
in
let rec loop max i ran =
  let im = 13998.0 in
  let ia = 3877.0 in
  let ic = 29573.0 in
  if i <= 2 then 0.0 else
    let last = ran.(0) in
    let last = mod_ ((last *. ia) +. ic) im in
    let new_last = (max *. last) /. im in
    ran.(0) <- new_last;
    loop max (i-1) ran
in
let n = 1000 in
let max = 100.0 in
let ran = Array.make 42 2.0 in
let s = get_current_micros () in
let _ = (loop max n ran) in
let e = get_current_micros () in
print_int (e - s); print_newline ()
