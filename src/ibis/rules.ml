open Types
open Is

module Engine = Rule_engine.Make (struct
  type t = Is.t
end)

module type DOMAIN = sig
  val relevant : answer -> question -> bool
  val combine : question -> answer -> prop
  val resolves : prop -> question -> bool
  val get_plan : question -> plan list option
end

let rec find_map f items =
  match items with
  | [] -> None
  | item :: rest -> (
      match f item with
      | Some result -> Some result
      | None -> find_map f rest)

let pop stack =
  match Stack.pop stack with
  | None -> stack
  | Some (_, rest) -> rest

module Make (Domain : DOMAIN) = struct
  type rule = Is.t Engine.rule

  let get_latest_moves ~latest_moves ~latest_speaker =
    Engine.rule
      ~name:"get_latest_moves"
      ~precondition:(fun state ->
        if state.shared.lu <> (latest_speaker, latest_moves) then
          Some ()
        else
          None)
      ~apply:(fun state () ->
        {
          state with
          shared = { state.shared with lu = (latest_speaker, latest_moves) };
        })

  let integrate_sys_ask =
    Engine.rule
      ~name:"integrate_sys_ask"
      ~precondition:(fun state ->
        match state.shared.lu with
        | SYS, moves ->
            find_map
              (function
                | Ask question -> Some question
                | _ -> None)
              moves
        | _ -> None)
      ~apply:(fun state question ->
        let qud = Stack.push question state.shared.qud in
        { state with shared = { state.shared with qud } })

  let integrate_usr_ask =
    Engine.rule
      ~name:"integrate_usr_ask"
      ~precondition:(fun state ->
        match state.shared.lu with
        | USR, moves ->
            find_map
              (function
                | Ask question -> Some question
                | _ -> None)
              moves
        | _ -> None)
      ~apply:(fun state question ->
        let qud = Stack.push question state.shared.qud in
        let agenda = Stack.push (Plan Respond) state.private_.agenda in
        {
          state with
          shared = { state.shared with qud };
          private_ = { state.private_ with agenda };
        })

  let integrate_answer =
    Engine.rule
      ~name:"integrate_answer"
      ~precondition:(fun state ->
        match Stack.peek state.shared.qud with
        | None -> None
        | Some question ->
            let _, moves = state.shared.lu in
            find_map
              (function
                | Answer answer
                  when Domain.relevant answer question ->
                    Some (question, answer)
                | _ -> None)
              moves)
      ~apply:(fun state (question, answer) ->
        let proposition = Domain.combine question answer in
        let com = Set.add proposition state.shared.com in
        { state with shared = { state.shared with com } })

  let integrate_greet =
    Engine.rule
      ~name:"integrate_greet"
      ~precondition:(fun state ->
        let _, moves = state.shared.lu in
        if List.exists (function Greet -> true | _ -> false) moves then
          Some ()
        else
          None)
      ~apply:(fun state () -> state)

  let integrate_sys_quit ~program_state =
    Engine.rule
      ~name:"integrate_sys_quit"
      ~precondition:(fun state ->
        match state.shared.lu with
        | SYS, moves ->
            if List.exists (function Quit -> true | _ -> false) moves then
              Some ()
            else
              None
        | _ -> None)
      ~apply:(fun state () ->
        program_state := QUIT;
        state)

  let integrate_usr_quit =
    Engine.rule
      ~name:"integrate_usr_quit"
      ~precondition:(fun state ->
        match state.shared.lu with
        | USR, moves ->
            if List.exists (function Quit -> true | _ -> false) moves then
              Some ()
            else
              None
        | _ -> None)
      ~apply:(fun state () ->
        let agenda = Stack.push (Move Quit) state.private_.agenda in
        { state with private_ = { state.private_ with agenda } })

  let downdate_qud =
    Engine.rule
      ~name:"downdate_qud"
      ~precondition:(fun state ->
        match Stack.peek state.shared.qud with
        | None -> None
        | Some question ->
            let resolved =
              state.shared.com
              |> Set.to_list
              |> List.exists (fun prop -> Domain.resolves prop question)
            in
            if resolved then Some () else None)
      ~apply:(fun state () ->
        let qud = pop state.shared.qud in
        { state with shared = { state.shared with qud } })

  let find_plan =
    Engine.rule
      ~name:"find_plan"
      ~precondition:(fun state ->
        match Stack.peek state.private_.agenda with
        | Some (Plan Respond) -> (
            match Stack.peek state.shared.qud with
            | None -> None
            | Some question ->
                let resolved =
                  state.private_.bel
                  |> Set.to_list
                  |> List.exists (fun prop -> Domain.resolves prop question)
                in
                if resolved then
                  None
                else
                  Domain.get_plan question)
        | _ -> None)
      ~apply:(fun state plan ->
        let agenda = pop state.private_.agenda in
        let plan_stack = Stack.of_list plan in
        { state with private_ = { state.private_ with agenda; plan = plan_stack } })

  let recover_plan =
    Engine.rule
      ~name:"recover_plan"
      ~precondition:(fun state ->
        if Stack.is_empty state.private_.agenda && Stack.is_empty state.private_.plan
        then
          match Stack.peek state.shared.qud with
          | None -> None
          | Some question -> Domain.get_plan question
        else
          None)
      ~apply:(fun state plan ->
        let plan_stack = Stack.of_list plan in
        { state with private_ = { state.private_ with plan = plan_stack } })

  let exec_plan = Engine.group ~name:"exec_plan" []

  let grounding ~latest_moves ~latest_speaker =
    Engine.group ~name:"grounding" [ get_latest_moves ~latest_moves ~latest_speaker ]

  let integrate ~program_state =
    Engine.group
      ~name:"integrate"
      [
        integrate_usr_ask;
        integrate_sys_ask;
        integrate_answer;
        integrate_greet;
        integrate_usr_quit;
        integrate_sys_quit ~program_state;
      ]

  let qud = Engine.group ~name:"qud" [ downdate_qud ]

  let plan = Engine.group ~name:"plan" [ recover_plan; find_plan ]

  let exec = exec_plan
end
