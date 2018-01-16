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
        "p", "(Prop p)";
        "(p)", "(Prop p)";
        "p & q", "(Conj (Prop p) (Prop q))";
        "p | q", "(Disj (Prop p) (Prop q))";
        "p -> q", "(Cond (Prop p) (Prop q))";
        "-p", "(Neg (Prop p))";
        "-(p & q)", "(Neg (Conj (Prop p) (Prop q)))";
        "-p & q", "(Conj (Neg (Prop p)) (Prop q))";
        "Ax p", "(Forall x (Prop p))";
        "Axp", "(Forall x (Prop p))";
        "Fx", "(Relation F (x))";
        "Fxy", "(Relation F (x y))";
        "Ax (Fx | -Fx)", "(Forall x (Disj (Relation F (x)) (Neg (Relation F (x)))))"
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
          assumptions = Set.singleton (module Int) 1;
          number = 1;
          expr = parse_string "p -> q";
          citations = [| |];
          rule = Deduction.PI;
        };

        "[2] 3. p -> q 1, 2 CI",
        Deduction.Line.{
          assumptions = Set.singleton (module Int) 2;
          number = 3;
          expr = parse_string "p -> q";
          citations = [| 1; 2 |];
          rule = Deduction.CI;
        };

        "[2] 3. Ax Fx | p 1, 2 CI",
        Deduction.Line.{
          assumptions = Set.singleton (module Int) 2;
          number = 3;
          expr = parse_string "Ax Fx | p";
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
    let prop c = Expression.Prop c

    let%test "Correct evaluation" =
      let model = Model.of_alist_exn [prop 'p', true; prop 'q', false] in
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
      let model = Model.of_alist_exn [prop 'p', true] in
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
          in let expected = List.map expected_letters ~f:(fun c -> prop c) in
          [%compare.equal: Expression.t list] letters expected)

    let%test "All" =
      let test_expressions = List.map ["p"; "q";] ~f:parse_string in
      let models = Model.all test_expressions in
      List.for_all models ~f:(fun model ->
          Option.is_some @@ Model.eval model @@ parse_string "p & q")
      && List.length models = 4

    let%test "Implies (positive)" =
      let assumptions = List.map ~f:parse_string ["p"; "p -> q"] in
      let conclusion = parse_string "q" in
      Model.implies assumptions conclusion

    let%test "Implies (FOL)" =
      let assumptions = List.map ~f:parse_string ["Fx"; "Fx -> Gx"] in
      let conclusion = parse_string "Gx" in
      Model.implies assumptions conclusion

    let%test "Implies (negative)" =
      let assumptions = List.map ~f:parse_string ["p"; "p -> q"] in
      let conclusion = parse_string "-q" in
      not @@ Model.implies assumptions conclusion
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
      |> function | Ok () -> true
                  | Error _ -> false

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

    let%test "UI" =
      test [|
        "[1] 1. Ax Fx PI";
        "[1] 2. Fx 1 UI";
      |]

    let%test "UG" =
      test [|
        "[1] 1. Fx PI";
        "[]  2. Fx -> Fx 1, 1 CI";
        "[] 2. Ax (Fx -> Fx) 2 UG";
      |]
      && not @@ test [|
        "[1] 1. Fx PI";
        "[1] 2. Ax Fx 1 UG"
      |]
  end)

let%test_module "FOL test" = (
  module
  struct
    let%test_module "create_instance" = (
      module
      struct
        let%test "basic" =
          let formula = parse_string "Ax Fx" in
          let instance = Fol.create_instance formula 'y' in
          Expression.equal instance @@ parse_string "Fy"

        let%test "Bound occurrence" =
          let formula = parse_string "Ax Ax Fx" in
          let instance = Fol.create_instance formula 'y' in
          Expression.equal instance @@ parse_string "Ax Fx"

        let%test "Replacement would be bound" =
          let formula = parse_string "Ax Ay Fx" in
          let instance = Fol.create_instance formula 'y' in
          Expression.equal instance @@ parse_string "Ay Fx"
      end)

    let%test_module "free_variables" = (
      module
      struct
        let test = fun input expected ->
          input
          |> parse_string
          |> Fol.free_variables
          |> [%compare.equal: char list] expected

        let%test "basic" =
          test "Fx" ['x']

        let%test "Bound occurrence" =
          test "Ax Ax Fx" []

        let%test "Multiple" =
          test "Ax Fy & Gz" ['y'; 'z']

        let%test "Proposition" =
          test "Fx | p" ['x']
      end)

    let%test_module "is_instance" = (
      module
      struct
        let test = fun big small ->
          let big, small = parse_string big, parse_string small in
          Fol.is_instance big small

        let%test "basic" = test "Ax Fx" "Fx"
        let%test "changed variable" = test "Ax Fx" "Fy"
        let%test "bound variable" = not @@ test "Ax Ax Fx" "Fx"
        let%test "different variable" = not @@ test "Ax Fy" "Fx"
        let%test "new variable would be bound" = not @@ test "Ax Ay Fx" "Ay Fy"
        let%test "no instances" = test "Ax p" "p"
      end)

  end)

let () =
  Ppx_inline_test_lib.Runtime.exit ()
