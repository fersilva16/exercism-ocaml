let leap_year year =
  let evenly_divisible_by n = year mod n == 0 in
    evenly_divisible_by 4 && (not (evenly_divisible_by 100) || evenly_divisible_by 400)
