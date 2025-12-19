open Types

type private_state = {
  agenda : agenda_item Stack.t;
  plan : plan Stack.t;
  bel : prop Set.t;
}

type shared_state = {
  com : prop Set.t;
  qud : question Stack.t;
  lu : speaker * move list;
}

type t = {
  private_ : private_state;
  shared : shared_state;
}

let empty =
  {
    private_ =
      {
        agenda = Stack.empty;
        plan = Stack.empty;
        bel = Set.empty;
      };
    shared =
      {
        com = Set.empty;
        qud = Stack.empty;
        lu = (SYS, []);
      };
  }

let validate_shared shared =
  match Stack.peek shared.qud with
  | None -> true
  | Some question ->
      assert (valid_question question);
      true

let validate state =
  let _ = validate_shared state.shared in
  state
