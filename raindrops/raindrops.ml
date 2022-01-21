let factor_3 n = if n mod 3 == 0 then "Pling" else ""

let factor_5 n = if n mod 5 == 0 then "Plang" else ""

let factor_7 n = if n mod 7 == 0 then "Plong" else ""

let raindrop n =
  let r = factor_3 n ^ factor_5 n ^ factor_7 n in
    if String.equal r "" then
      Int.to_string n
    else
      r
