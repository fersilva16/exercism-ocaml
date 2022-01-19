type nucleotide = A | C | G | T

let hamming_distance a b = match a, b with
  | [], [_] -> Error "left strand must not be empty"
  | [_], [] -> Error "right strand must not be empty"
  | a, b when List.length a != List.length b -> Error "left and right strands must be of equal length"
  | a, b ->
    let f ca cb c = if ca == cb then c else c + 1 in
      Ok (List.fold_right2 f a b 0)
