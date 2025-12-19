open Types

let join sep items =
  String.concat sep items

let speaker = function
  | USR -> "USR"
  | SYS -> "SYS"

let program_state = function
  | RUN -> "RUN"
  | QUIT -> "QUIT"

let entity = function
  | Entity id -> Printf.sprintf "entity_%d" id

let term = function
  | Var id -> Printf.sprintf "?v%d" id
  | Entity_ref ent -> entity ent

let predicate = function
  | Predicate id -> Printf.sprintf "pred_%d" id

let rec prop = function
  | Atom (pred, terms) ->
      let args = terms |> List.map term |> join ", " in
      Printf.sprintf "%s(%s)" (predicate pred) args
  | Eq (left, right) -> Printf.sprintf "%s = %s" (term left) (term right)
  | Not inner -> Printf.sprintf "not(%s)" (prop inner)
  | And props -> Printf.sprintf "(%s)" (join " & " (List.map prop props))
  | Or props -> Printf.sprintf "(%s)" (join " | " (List.map prop props))

let wh = function
  | Who -> "who"
  | What -> "what"
  | Where -> "where"
  | When -> "when"
  | Which -> "which"

let question = function
  | Yes_no proposition -> Printf.sprintf "?%s" (prop proposition)
  | Wh (wh_word, proposition) ->
      Printf.sprintf "?%s(%s)" (wh wh_word) (prop proposition)

let answer = function
  | Short terms -> Printf.sprintf "[%s]" (join ", " (List.map term terms))
  | Full proposition -> prop proposition
  | Unknown -> "unknown"

let content = function
  | Question q -> question q
  | Answer a -> answer a
  | Prop p -> prop p

let icm = function
  | ICM_ack -> "icm_ack"
  | ICM_reject -> "icm_reject"
  | ICM_clarify question -> Printf.sprintf "icm_clarify(%s)" (question question)
  | ICM_inform proposition -> Printf.sprintf "icm_inform(%s)" (prop proposition)

let move = function
  | Greet -> "greet"
  | Ask question -> Printf.sprintf "ask(%s)" (question question)
  | Answer answer -> Printf.sprintf "answer(%s)" (answer answer)
  | ICM icm_move -> Printf.sprintf "icm(%s)" (icm icm_move)
  | Quit -> "quit"

let rec plan = function
  | Respond -> "respond"
  | Consult_db -> "consult_db"
  | Findout question -> Printf.sprintf "findout(%s)" (question question)
  | Raise question -> Printf.sprintf "raise(%s)" (question question)
  | If (condition, then_plans, else_plans) ->
      Printf.sprintf
        "if %s then [%s] else [%s]"
        (prop condition)
        (join ", " (List.map plan then_plans))
        (join ", " (List.map plan else_plans))
