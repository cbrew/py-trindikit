type prop =
  | Destination of string
  | Travel_date of string
  | Budget of int
  | Not of prop

type question =
  | Ask_destination
  | Ask_date
  | Ask_budget
  | Ask_prop of prop
  | Ask_alternatives of prop list

type answer =
  | Destination_answer of string
  | Date_answer of string
  | Budget_answer of int
  | Yes
  | No
  | Prop of prop

let relevant answer question =
  match question with
  | Ask_destination -> (
      match answer with
      | Destination_answer _ -> true
      | Prop (Destination _) -> true
      | _ -> false)
  | Ask_date -> (
      match answer with
      | Date_answer _ -> true
      | Prop (Travel_date _) -> true
      | _ -> false)
  | Ask_budget -> (
      match answer with
      | Budget_answer _ -> true
      | Prop (Budget _) -> true
      | _ -> false)
  | Ask_prop prop -> (
      match answer with
      | Yes | No -> true
      | Prop other -> other = prop
      | _ -> false)
  | Ask_alternatives props -> (
      match answer with
      | Prop candidate -> List.exists (( = ) candidate) props
      | _ -> false)

let combine question answer =
  if not (relevant answer question) then
    invalid_arg "combine: answer is not relevant to question";
  match question with
  | Ask_destination -> (
      match answer with
      | Destination_answer destination -> Destination destination
      | Prop (Destination destination) -> Destination destination
      | _ -> invalid_arg "combine: expected destination")
  | Ask_date -> (
      match answer with
      | Date_answer date -> Travel_date date
      | Prop (Travel_date date) -> Travel_date date
      | _ -> invalid_arg "combine: expected travel date")
  | Ask_budget -> (
      match answer with
      | Budget_answer budget -> Budget budget
      | Prop (Budget budget) -> Budget budget
      | _ -> invalid_arg "combine: expected budget")
  | Ask_prop prop -> (
      match answer with
      | Yes -> prop
      | No -> Not prop
      | Prop other -> other
      | _ -> invalid_arg "combine: expected yes/no or proposition")
  | Ask_alternatives _ -> (
      match answer with
      | Prop prop -> prop
      | _ -> invalid_arg "combine: expected proposition")

let resolve_plan question =
  match question with
  | Ask_destination -> [ Ask_date; Ask_budget ]
  | Ask_date -> [ Ask_destination; Ask_budget ]
  | Ask_budget -> [ Ask_destination; Ask_date ]
  | Ask_prop _ -> []
  | Ask_alternatives _ -> []
