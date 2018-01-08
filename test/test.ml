open Base

let%test_module "parser tests" = (
  module
  struct
    open Deductive_logic

    let equal = [%compare.equal: Expression.t]

    let parse_string s =
      s
      |> Lexing.from_string
      |> Parser.line Lexer.read

    let test_cases = [
      "p", "(Prop p)";
      "(p)", "(Prop p)";
      "p & q", "(Conj (Prop p) (Prop q))";
      "p | q", "(Disj (Prop p) (Prop q))";
      "p -> q", "(Cond (Prop p) (Prop q))";
      "-p", "(Neg (Prop p))";
      "-(p & q)", "(Neg (Conj (Prop p) (Prop q)))";
      "-p & q", "(Conj (Neg (Prop p)) (Prop q))";
    ]

    let%test _ = List.for_all test_cases ~f:(fun (input, output) ->
        equal (parse_string input) (Sexplib.Sexp.of_string output |> Expression.t_of_sexp)
      )
  end)

let () =
  Ppx_inline_test_lib.Runtime.exit ()
