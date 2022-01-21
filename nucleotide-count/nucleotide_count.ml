open Base

let empty = Map.empty (module Char)

let is_valid_nucleotide c = match c with
  | 'A' -> true
  | 'C' -> true
  | 'G' -> true
  | 'T' -> true
  | _   -> false

let has_invalid_nucleotide s = String.find s ~f:(Fn.compose not is_valid_nucleotide)

let count_nucleotide s c =
  let invalid_nucleotide = has_invalid_nucleotide s in
    match s, c with
      | _, _ when Option.is_some invalid_nucleotide -> Error (Option.value_exn invalid_nucleotide)
      | _, _ when not (is_valid_nucleotide c) -> Error c
      | _, _ -> let f a sc = if Char.equal sc c then a + 1 else a in
        Ok (String.fold s ~init:0 ~f)

let count_nucleotides s =
  let invalid_nucleotide = has_invalid_nucleotide s in
    if Option.is_some invalid_nucleotide then
      Error (Option.value_exn invalid_nucleotide)
    else
      let f map sc = Map.update map sc ~f:(fun data -> (Option.value data ~default:0) + 1) in
        Ok (String.fold s ~init:empty ~f)
