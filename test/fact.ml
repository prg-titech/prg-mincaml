let rec fact x =
  if x <= 1 then 1 else
    x * fact (x-1) in
let r = fact 4000 in
print_int r
