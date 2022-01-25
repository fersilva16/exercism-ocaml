open Base

let is_anagram s1 s2 = match List.fold2 s1 s2 ~f:(fun c a b -> c && Char.equal a b) ~init:true with
  | List.Or_unequal_lengths.Ok true -> true
  | _ -> false

let string_sort s = String.lowercase s |> String.to_list |> List.sort ~compare:Char.compare

let anagrams s sl =
  let cl = string_sort s in
    Caml.List.find_all (fun ls -> String.Caseless.equal s ls |> not && (string_sort ls |> is_anagram cl)) sl

