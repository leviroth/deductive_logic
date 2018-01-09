open Base
open Deductive_logic

let parse_string s =
  s
  |> Lexing.from_string
  |> Parser.expr_only Lexer.read


let%test_module "Parser tests" = (
  module
  struct
    let%test "Expression parser" =
      let test_cases = [
        "p", "(Atom (Prop p))";
        "(p)", "(Atom (Prop p))";
        "p & q", "(Conj (Atom (Prop p)) (Atom (Prop q)))";
        "p | q", "(Disj (Atom (Prop p)) (Atom (Prop q)))";
        "p -> q", "(Cond (Atom (Prop p)) (Atom (Prop q)))";
        "-p", "(Neg (Atom (Prop p)))";
        "-(p & q)", "(Neg (Conj (Atom (Prop p)) (Atom (Prop q))))";
        "-p & q", "(Conj (Neg (Atom (Prop p))) (Atom (Prop q)))";
        "Axp", "(Forall x (Atom (Prop p)))";
        "F(x)", "(Atom (Relation F (x)))";
        "Ax (F(x) | -F(x))", "(Forall x (Disj (Atom (Relation F (x))) (Neg (Atom (Relation F (x))))))"
      ]
      in
      List.for_all test_cases ~f:(fun (formula, sexp) ->
          let expected =
            sexp
            |> Sexplib.Sexp.of_string
            |> Expression.t_of_sexp
          in
          Expression.equal (parse_string formula) expected)

    let%test "Deduction parser" =
      let test_cases = [
        "[1] 1. p -> q PI",
        Deduction.Line.{
          premises = Set.singleton (module Int) 1;
          number = 1;
          expr = parse_string "p -> q";
          citations = [| |];
          rule = Deduction.PI;
        };

        "[2] 3. p -> q 1, 2 CI",
        Deduction.Line.{
          premises = Set.singleton (module Int) 2;
          number = 3;
          expr = parse_string "p -> q";
          citations = [| 1; 2 |];
          rule = Deduction.CI;
        };

        "[2] 3. Ax F(x) | p 1, 2 CI",
        Deduction.Line.{
          premises = Set.singleton (module Int) 2;
          number = 3;
          expr = parse_string "Ax F(x) | p";
          citations = [| 1; 2 |];
          rule = Deduction.CI;
        };
      ]
      in
      List.for_all test_cases ~f:(fun (line_string, expected) ->
          let line =
            line_string
            |> Lexing.from_string
            |> Parser.deduction_line_only Lexer.read
          in
          [%compare.equal: Deduction.Line.t]
            line
            expected)
  end)

let%test_module "Model tests" = (
  module
  struct
    let%test "Correct evaluation" =
      let model = Model.of_alist_exn [Expression.Atom.Prop 'p', true; Expression.Atom.Prop 'q', false] in
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

    let%test "Check model completeness" =
      let model = Model.of_alist_exn [Expression.Atom.Prop 'p', true] in
      Option.is_none @@ Model.eval model @@ parse_string "p & q"

    let%test "Letters_used" =
      let test_cases = [
        "p", ['p'];
        "(p)", ['p'];
        "p & q", ['p'; 'q'];
        "-p", ['p'];
        "p & q & p", ['p'; 'q'];
      ]
      in
      List.for_all test_cases ~f:(fun (formula, expected_letters) ->
          let letters =
            formula
            |> parse_string
            |> Model.letters_used
            |> Set.to_list
          in let expected = List.map expected_letters ~f:(fun c -> Expression.Atom.Prop c) in
          [%compare.equal: Expression.Atom.t list] letters expected)

    let%test "All" =
      let test_expressions = List.map ["p"; "q";] ~f:parse_string in
      let models = Model.all test_expressions in
      List.for_all models ~f:(fun model ->
          Option.is_some @@ Model.eval model @@ parse_string "p & q")
      && List.length models = 4

    let%test "Implies (positive)" =
      let premises = List.map ~f:parse_string ["p"; "p -> q"] in
      let conclusion = parse_string "q" in
      Model.implies premises conclusion

    let%test "Implies (FOL)" =
      let premises = List.map ~f:parse_string ["F(x)"; "F(x) -> G(x)"] in
      let conclusion = parse_string "G(x)" in
      Model.implies premises conclusion

    let%test "Implies (negative)" =
      let premises = List.map ~f:parse_string ["p"; "p -> q"] in
      let conclusion = parse_string "-q" in
      not @@ Model.implies premises conclusion
  end)

let%test_module "Deduction test" = (
  module
  struct
    let construct_deduction = Array.map ~f:(fun s ->
        s
        |> Lexing.from_string
        |> Parser.deduction_line_only Lexer.read)

    let test a =
      construct_deduction a
      |> Deduction.validate
      |> function | Ok true -> true
                  | Ok false | Error _ -> false

    let%test "PI" =
      test [|
        "[1] 1. p -> q  PI"
      |]

    let%test "CI" =
      test [|
        "[1] 1. p PI";
        "[2] 2. q PI";
        "[2] 3. p -> q 1, 2 CI"
      |]

    let%test "CE" =
      test [|
        "[1] 1. p PI";
        "[2] 2. p -> q PI";
        "[1, 2] 3. q 1, 2 CE"
      |]

    let%test "NI" =
      test [|
        "[1] 1. p PI";
        "[2] 2. -p PI";
        "[3] 3. q PI";
        "[1, 2] 4. -q 1, 2, 3 NI"
      |]

    let%test "NE" =
      test [|
        "[1] 1. p PI";
        "[2] 2. -p PI";
        "[3] 3. -q PI";
        "[1, 2] 4. q 1, 2, 3 NE"
      |]

    let%test "JI" =
      test [|
        "[1] 1. p PI";
        "[2] 2. q PI";
        "[1, 2] 3. p & q 1, 2 JI";
      |]

    let%test "JE" =
      test [|
        "[1] 1. p & q PI";
        "[1] 2. q 1 JE";
      |]

    let%test "DI" =
      test [|
        "[1] 1. p PI";
        "[1] 2. q | p 1 DI";
      |]

    let%test "DE" =
      test [|
        "[1] 1. p | q PI";
        "[2] 2. -p PI";
        "[1, 2] 3. q 1, 2 DE";
      |]
  end
)


let () =
  Ppx_inline_test_lib.Runtime.exit ()
