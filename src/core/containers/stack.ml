type 'a t = 'a list

let empty = []

let is_empty stack =
  stack = []

let push item stack =
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
  items

let size stack =
  List.length stack

let for_all predicate stack =
  List.for_all predicate stack

let map f stack =
  List.map f stack
