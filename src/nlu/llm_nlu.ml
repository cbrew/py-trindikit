open Sig

module type LLM = sig
  type move

  val system_prompt : string
  val call : string -> string
  val parse_moves : string -> move list
end

module Make (Model : LLM) : NLU with type move = Model.move = struct
  type move = Model.move

  let raw_text_of_input = function
    | Raw_text text -> text
    | Tokens tokens ->
        tokens
        |> List.map (fun (Token token) -> token)
        |> String.concat " "

  let interpret input =
    let user_text = raw_text_of_input input in
    let prompt = Model.system_prompt ^ "\n\n" ^ user_text in
    let response = Model.call prompt in
    Model.parse_moves response
end
