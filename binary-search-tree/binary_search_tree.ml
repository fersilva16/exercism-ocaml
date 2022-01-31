open Base

type bst =
  | Leaf
  | Node of bst * int * bst

let empty = Leaf

let value = function
  | Leaf            -> Error "Tree is empty"
  | (Node(_, v, _)) -> Ok v

let left = function
  | Leaf            -> Error "Tree is empty"
  | (Node(l, _, _)) -> Ok l

let right = function
  | Leaf            -> Error "Tree is empty"
  | (Node(_, _, r)) -> Ok r

let rec insert n = function
  | Leaf                     -> Node(Leaf, n, Leaf)
  | Node(l, v, r) when n > v -> Node(l, v, insert n r)
  | Node(l, v, r)            -> Node(insert n l, v, r)

let rec to_list = function
  | Leaf          -> []
  | Node(l, v, r) -> List.concat [to_list l; [v]; to_list r]
