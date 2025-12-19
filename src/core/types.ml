type speaker =
  | USR
  | SYS

type program_state =
  | RUN
  | QUIT

type entity =
  | Entity of int

type term =
  | Var of int
  | Entity_ref of entity

type predicate =
  | Predicate of int

type prop =
  | Atom of predicate * term list
  | Eq of term * term
  | Not of prop
  | And of prop list
  | Or of prop list

type wh =
  | Who
  | What
  | Where
  | When
  | Which

type question =
  | Yes_no of prop
  | Wh of wh * prop

type answer =
  | Short of term list
  | Full of prop
  | Unknown

type content =
  | Question of question
  | Answer of answer
  | Prop of prop

type icm =
  | ICM_ack
  | ICM_reject
  | ICM_clarify of question
  | ICM_inform of prop

type move =
  | Greet
  | Ask of question
  | Answer of answer
  | ICM of icm
  | Quit

type plan =
  | Respond
  | Consult_db
  | Findout of question
  | Raise of question
  | If of prop * plan list * plan list

type agenda_item =
  | Plan of plan
  | Move of move

let rec valid_prop prop =
  match prop with
  | Atom _ -> true
  | Eq _ -> true
  | Not inner -> valid_prop inner
  | And props
  | Or props ->
      props <> [] && List.for_all valid_prop props

let valid_question question =
  match question with
  | Yes_no prop -> valid_prop prop
  | Wh (_, prop) -> valid_prop prop

let valid_answer answer =
  match answer with
  | Short _ -> true
  | Full prop -> valid_prop prop
  | Unknown -> true
