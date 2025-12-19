module type DOMAIN = sig
  type state
  type move
  type question
  type answer
  type prop

  val relevant : state -> question -> answer -> bool
  val combine : state -> answer -> state
  val resolve_plan : state -> state
  val equal_state : state -> state -> bool
end

module Make (D : DOMAIN) = struct
  type state = D.state
  type move = D.move
  type question = D.question
  type answer = D.answer
  type prop = D.prop

  type rule = {
    name : string;
    precondition : state -> bool;
    apply : state -> state;
  }

  type tracer = {
    on_apply : rule -> state -> state -> unit;
    on_skip : rule -> state -> unit;
  }

  let make_rule ?(name = "anonymous") ~precondition ~apply =
    { name; precondition; apply }

  let maybe_apply ?trace rule state =
    if rule.precondition state then
      let next_state = rule.apply state in
      Option.iter (fun hook -> hook.on_apply rule state next_state) trace;
      Some next_state
    else (
      Option.iter (fun hook -> hook.on_skip rule state) trace;
      None)

  let apply_rule ?trace rule state =
    match maybe_apply ?trace rule state with
    | Some next_state -> next_state
    | None -> state

  let apply_all ?trace state rules =
    List.fold_left (fun current rule -> apply_rule ?trace rule current) state rules

  let rule_group ?(name = "rule_group") rules =
    let precondition state =
      List.exists (fun rule -> rule.precondition state) rules
    in
    let apply state = apply_all state rules in
    { name; precondition; apply }

  let repeat_until_fixpoint ?max_steps ?trace ?(equal = D.equal_state) rules state =
    let rec loop steps current =
      let next_state = apply_all ?trace current rules in
      if equal current next_state then
        current
      else
        match max_steps with
        | Some max when steps >= max -> next_state
        | _ -> loop (steps + 1) next_state
    in
    loop 0 state
end
