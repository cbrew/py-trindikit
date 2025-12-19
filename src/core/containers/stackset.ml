type 'a t = 'a list

let empty = []

let mem item stack =
  List.exists (( = ) item) stack

let push item stack =
  if mem item stack then
    stack
  else
    item :: stack

let pop stack =
  match stack with
  | [] -> None
  | head :: tail -> Some (head, tail)

let peek stack =
  match stack with
  | [] -> None
  | head :: _ -> Some head

let to_list stack =
  stack

let of_list items =
  List.fold_right push items empty

let size stack =
  List.length stack
