open Base

module Int_map = Map.M(Int)
type school = string list Int_map.t

let empty_school = Map.empty (module Int)

let grade g sc = match Map.find sc g with
  | None    -> []
  | Some ss -> ss

let add s g sc =
  let ss = List.append (grade g sc) [s] in
  let sorted_ss = List.sort ~compare:String.compare ss in
  Map.set sc ~key:g ~data:sorted_ss

(* Map is already sorted *)
let sorted sc = sc

let roster sc =
  let list = Map.to_alist sc ~key_order:`Increasing in
  List.fold list ~init:[] ~f:(fun acc (_, b) -> List.append acc b)
