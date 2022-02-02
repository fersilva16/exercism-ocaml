open Base

let response_for s =
  let has_nothing = String.for_all s ~f:Char.is_whitespace in
  let has_letter = String.exists s ~f:Char.is_alpha in
  let trimmed = Caml.String.trim s in
  let is_screaming = String.for_all trimmed ~f:(fun c -> Char.equal (Char.uppercase c) c) in
  let is_question = String.is_suffix trimmed ~suffix:"?" in
    if has_nothing then "Fine. Be that way!"
    else if has_letter && is_screaming && is_question then "Calm down, I know what I'm doing!"
    else if has_letter && is_screaming then "Whoa, chill out!"
    else if is_question then "Sure."
    else "Whatever."
