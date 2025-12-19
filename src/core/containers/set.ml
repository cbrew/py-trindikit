type 'a t = 'a list

let empty = []

let mem item set =
  List.exists (( = ) item) set

let add item set =
  if mem item set then
    set
  else
    item :: set

let remove item set =
  List.filter (fun value -> value <> item) set

let to_list set =
  set

let of_list items =
  List.fold_left (fun acc item -> add item acc) empty items

let size set =
  List.length set

let union left right =
  List.fold_left (fun acc item -> add item acc) left right

let inter left right =
  List.filter (fun item -> mem item right) left

let diff left right =
  List.filter (fun item -> not (mem item right)) left
