module TravelDomain = Travel_domain
module TravelRuleEngine = RuleEngine.Make (TravelDomain)
module TravelSystem = System.Make (TravelRuleEngine) (LLM_NLU) (LLM_NLG)

let rec cli_loop system =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | "quit" | "exit" -> ()
  | input ->
      let output, next_system = TravelSystem.handle_input system input in
      print_endline output;
      cli_loop next_system

let () =
  let system = TravelSystem.init () in
  cli_loop system
