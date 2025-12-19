type surface_token =
  | Surface_token of string

type output =
  | Tokens of surface_token list
  | Raw_text of string

module type NLG = sig
  type move

  val generate : move list -> output
end
