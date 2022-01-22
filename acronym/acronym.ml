open Base

let is_space = function
  | ' ' -> true
  | '_' -> true
  | '-' -> true
  | _   -> false

let rec skip_spaces s =
  if String.is_empty s |> not && String.get s 0 |> is_space then String.drop_prefix s 1 |> skip_spaces
  else s

let rec skip_word s =
  if String.is_empty s || String.get s 0 |> is_space then s
  else String.drop_prefix s 1 |> skip_word

let rec acronym s =
  if String.is_empty s then ""
  else (String.get s 0 |> Char.uppercase |> Char.to_string) ^ (skip_word s |> skip_spaces |> acronym)
