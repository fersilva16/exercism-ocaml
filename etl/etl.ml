let transform scores =
  let transform_score r (s, ls) = List.fold_left (fun lr l -> List.append lr [(Char.lowercase_ascii l, s)]) r ls in
  List.fold_left transform_score [] scores |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
