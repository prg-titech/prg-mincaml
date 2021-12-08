let rec quick_sort xs len =
  let rec swap i j =
    let tmp = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- tmp
  in
  let rec go left right =
    if right <= left
    then ()
    else (
      let l = left + right in
      let pivot = xs.(l / 2) in
      let rec partition l r =
        let rec next_left i =
          (* pivot <= xs.(i) *)
          if pivot <= xs.(i) then i else next_left (i + 1)
        in
        let rec next_right i =
          (* pivot >= xs.(i) *)
          if xs.(i) < pivot then i else next_right (i - 1)
        in
        let l = next_left l in
        let r = next_right r in
        if l >= r
        then l, r
        else (
          swap l r;
          partition (l + 1) (r - 1))
      in
      let (l, r) = partition left right in
      go left (l - 1);
      go (r + 1) right)
  in
  go 0 (len - 1);
  xs
in
let a = Array.make 5 0 in
a.(0) <- 4;
a.(1) <- 8;
a.(2) <- 1;
a.(3) <- 1;
a.(4) <- 5;
let sorted = quick_sort a 5 in
print_int (sorted.(0))
