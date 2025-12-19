open Types

module Mivs = struct
  type t = {
    input : string option;
    latest_speaker : speaker;
    latest_moves : move Set.t;
    next_moves : move Stack.t;
    output : string option;
    program_state : program_state;
  }

  let init =
    {
      input = None;
      latest_speaker = SYS;
      latest_moves = Set.empty;
      next_moves = Stack.empty;
      output = None;
      program_state = RUN;
    }
end

type state = {
  is : Is.t;
  mivs : Mivs.t;
}

module type RuleEngine = sig
  val init_is : Is.t
  val select : Is.t -> Mivs.t -> Is.t * Mivs.t
  val update : Is.t -> Mivs.t -> Is.t * Mivs.t
end

module type NLU = sig
  val interpret : string -> move list
end

module type NLG = sig
  val generate : move list -> string
end

module Make (RuleEngine : RuleEngine) (NLU : NLU) (NLG : NLG) = struct
  type trace_hook = string -> unit

  let format_list items =
    let joined = String.concat ", " items in
    Printf.sprintf "[%s]" joined

  let agenda_item = function
    | Plan plan_item -> Printf.sprintf "Plan(%s)" (Pretty.plan plan_item)
    | Move move_item -> Printf.sprintf "Move(%s)" (Pretty.move move_item)

  let render_mivs mivs =
    [
      Printf.sprintf
        "INPUT:         %s"
        (Option.value ~default:"" mivs.input);
      Printf.sprintf "LATEST_SPEAKER: %s" (Pretty.speaker mivs.latest_speaker);
      Printf.sprintf
        "LATEST_MOVES:  %s"
        (mivs.latest_moves |> Set.to_list |> List.map Pretty.move |> format_list);
      Printf.sprintf
        "NEXT_MOVES:    %s"
        (mivs.next_moves |> Stack.to_list |> List.map Pretty.move |> format_list);
      Printf.sprintf
        "OUTPUT:        %s"
        (Option.value ~default:"" mivs.output);
      Printf.sprintf "PROGRAM_STATE: %s" (Pretty.program_state mivs.program_state);
    ]

  let render_is state =
    let private_ = state.is.private_ in
    let shared = state.is.shared in
    [
      Printf.sprintf
        "IS.private.agenda: %s"
        (private_.agenda |> Stack.to_list |> List.map agenda_item |> format_list);
      Printf.sprintf
        "IS.private.plan:   %s"
        (private_.plan |> Stack.to_list |> List.map Pretty.plan |> format_list);
      Printf.sprintf
        "IS.private.bel:    %s"
        (private_.bel |> Set.to_list |> List.map Pretty.prop |> format_list);
      Printf.sprintf
        "IS.shared.com:     %s"
        (shared.com |> Set.to_list |> List.map Pretty.prop |> format_list);
      Printf.sprintf
        "IS.shared.qud:     %s"
        (shared.qud |> Stack.to_list |> List.map Pretty.question |> format_list);
      Printf.sprintf
        "IS.shared.lu:      (%s, %s)"
        (Pretty.speaker (fst shared.lu))
        (snd shared.lu |> List.map Pretty.move |> format_list);
    ]

  let print_state state =
    print_endline "+------------------------ - -  -";
    render_mivs state.mivs |> List.iter (fun line -> print_endline ("| " ^ line));
    print_endline "|";
    render_is state |> List.iter (fun line -> print_endline ("| " ^ line));
    print_endline "+------------------------ - -  -";
    print_endline ""

  let trace trace_hook message =
    trace_hook (Printf.sprintf "{%s}" message)

  let generate state =
    let moves = state.mivs.next_moves |> Stack.to_list in
    let output = NLG.generate moves in
    { state with mivs = { state.mivs with output = Some output } }

  let output state =
    let output_text = Option.value ~default:"[---]" state.mivs.output in
    print_endline ("S> " ^ output_text);
    print_endline "";
    let latest_moves = state.mivs.next_moves |> Stack.to_list |> Set.of_list in
    {
      state with
      mivs =
        {
          state.mivs with
          latest_speaker = SYS;
          latest_moves;
          next_moves = Stack.empty;
        };
    }

  let input state =
    print_string "U> ";
    flush stdout;
    match read_line () with
    | line ->
        print_endline "";
        {
          state with
          mivs =
            {
              state.mivs with
              input = Some line;
              latest_speaker = USR;
            };
        }
    | exception End_of_file ->
        print_endline "";
        {
          state with
          mivs = { state.mivs with input = None; program_state = QUIT };
        }

  let interpret state =
    match state.mivs.input with
    | None ->
        {
          state with
          mivs = { state.mivs with latest_moves = Set.empty };
        }
    | Some input_text ->
        let moves = NLU.interpret input_text in
        {
          state with
          mivs = { state.mivs with latest_moves = Set.of_list moves };
        }

  let update state =
    let is, mivs = RuleEngine.update state.is state.mivs in
    { is; mivs }

  let select state =
    let is, mivs = RuleEngine.select state.is state.mivs in
    { is; mivs }

  let run ?(trace_hook = fun _ -> ()) ?(on_state = fun _ -> ()) () =
    let state = { is = RuleEngine.init_is; mivs = Mivs.init } in
    on_state state;
    let rec loop state =
      trace trace_hook "select";
      let state = select state in
      trace trace_hook "generate";
      let state = generate state in
      trace trace_hook "output";
      let state = output state in
      trace trace_hook "update";
      let state = update state in
      on_state state;
      if state.mivs.program_state = QUIT then
        ()
      else (
        trace trace_hook "input";
        let state = input state in
        trace trace_hook "interpret";
        let state = interpret state in
        trace trace_hook "update";
        let state = update state in
        on_state state;
        loop state)
    in
    loop state
end
