let%test_module "parser tests" = (
  module
  struct
    open Deductive_logic
    open Expression

    let equal = [%compare.equal: Expression.t]

    let parse_string s =
      s
      |> Lexing.from_string
      |> Parser.line Lexer.read

    let%test _ = equal (parse_string "-(p & q)") (Neg (Conj (Prop 'p', Prop 'q')))
  end)

let () =
  Ppx_inline_test_lib.Runtime.exit ()
