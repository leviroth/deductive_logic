open Base
open Deductive_logic

let parse_string s =
  s
  |> Lexing.from_string
  |> Parser.line Lexer.read


let%test_module "parser tests" = (
  module
  struct
    let equal = [%compare.equal: Expression.t]

    let%test _ =
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
      in
      List.for_all test_cases ~f:(fun (formula, sexp) ->
          let expected =
            sexp
            |> Sexplib.Sexp.of_string
            |> Expression.t_of_sexp
          in
          equal (parse_string formula) expected)
  end)

let%test_module "model tests" = (
  module
  struct
    let%test "correct evaluation" =
      let model = Model.of_alist_exn ['p', true; 'q', false] in
      let test_cases = [
        "p", true;
        "(p)", true;
        "p & q", false;
        "p | q", true;
        "p -> q", false;
        "-p", false;
        "-(p & q)", true;
        "-p & q", false;
      ]
      in
      List.for_all test_cases ~f:(fun (formula, expected) ->
          let value =
            formula
            |> parse_string
            |> Model.eval model
            |> Option.value_exn
          in
          Bool.equal value expected)

    let%test "check model completeness" =
      let model = Model.of_alist_exn ['p', true] in
      Option.is_none @@ Model.eval model @@ parse_string "p & q"

    let%test "letters_used" =
      let test_cases = [
        "p", ['p'];
        "(p)", ['p'];
        "p & q", ['p'; 'q'];
        "-p", ['p'];
        "p & q & p", ['p'; 'q'];
      ]
      in
      List.for_all test_cases ~f:(fun (formula, expected) ->
          let letters =
            formula
            |> parse_string
            |> Model.letters_used
            |> Set.to_list in
          [%compare.equal: char list] letters expected)

    let%test "all" =
      let test_expressions = List.map ["p"; "q";] ~f:parse_string in
      let models = Model.all test_expressions in
      List.for_all models ~f:(fun model ->
          Option.is_some @@ Model.eval model @@ parse_string "p & q")
      && List.length models = 4
  end)

let () =
  Ppx_inline_test_lib.Runtime.exit ()
