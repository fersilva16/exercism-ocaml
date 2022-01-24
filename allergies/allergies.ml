type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let scores = [
  (Cats, 128);
  (Pollen, 64);
  (Chocolate, 32);
  (Tomatoes, 16);
  (Strawberries, 8);
  (Shellfish, 4);
  (Peanuts, 2);
  (Eggs, 1);
]

let get_allergie s = List.find_opt (fun (_, asc) -> asc <= s) scores

let rec allergies s = match s with
  | 0 -> []
  | _ -> match get_allergie s with
    | None -> []
    | Some (a, asc) ->
      let l = allergies (s - asc) in
        match List.find_opt (fun al -> al == a) l with
          | None -> List.append l [a]
          | _    -> l

let allergic_to s a = match List.find_opt (fun al -> al == a) (allergies s) with
  | None -> false
  | _    -> true
