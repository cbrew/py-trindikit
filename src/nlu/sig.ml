type token =
  | Token of string

type input =
  | Tokens of token list
  | Raw_text of string

module type NLU = sig
  type move

  val interpret : input -> move list
end
