let rec f arr i j =
    let x = arr.(i) in
    arr.(j) <- x;
    arr.(i) + arr.(j)
in
let arr = Array.make 3 0 in
print_int (f arr 1 2)
