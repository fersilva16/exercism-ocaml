let rec triangular_number n = match n with
    0 -> 0
  | _ -> n + triangular_number (n - 1)

let square_of_sum n =
  let tn = triangular_number n in tn * tn

let rec sum_of_squares n = match n with
    0 -> 0
  | _ -> n * n + sum_of_squares (n - 1)

let difference_of_squares n = square_of_sum n - sum_of_squares n
