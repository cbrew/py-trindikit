open Sig

module type LLM = sig
  type move

  val system_prompt : string
  val render_moves : move list -> string
  val call : string -> string
  val output_of_response : string -> output
end

module Make (Model : LLM) : NLG with type move = Model.move = struct
  type move = Model.move

  let generate moves =
    let move_text = Model.render_moves moves in
    let prompt = Model.system_prompt ^ "\n\n" ^ move_text in
    let response = Model.call prompt in
    Model.output_of_response response
end
