type 'state rule = 'state -> 'state option

module type S = sig
  type state

  val rule :
    name:string ->
    precondition:(state -> 'a option) ->
    apply:(state -> 'a -> state) ->
    state rule

  val group : name:string -> state rule list -> state rule
end

module Make (State : sig
  type t
end) : S with type state = State.t = struct
  type state = State.t

  let rule ~name:_ ~precondition ~apply =
    fun state ->
      match precondition state with
      | None -> None
      | Some payload -> Some (apply state payload)

  let group ~name:_ rules =
    let rec apply_rules state remaining =
      match remaining with
      | [] -> None
      | rule :: rest -> (
          match rule state with
          | None -> apply_rules state rest
          | Some updated -> Some updated)
    in
    fun state -> apply_rules state rules
end
