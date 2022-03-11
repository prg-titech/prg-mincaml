let rec tarai x y z =
    if x <= y then y
    else tarai (tarai (x-1) y z) (tarai (y-1) z x) (tarai (z-1) x y)
in print_int (tarai 12 5 0)
