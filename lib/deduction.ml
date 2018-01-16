open Base

type rule = PI | CI | CE | NI | NE | JI | JE | DI | DE | UI | UG [@@deriving compare, yojson]

module Line = struct
  type t = { assumptions : Set.M(Int).t;
             number : int;
             expr : Expression.t;
             citations : int array;
             rule : rule; } [@@deriving compare]

  let get_line deduction n = deduction.(n - 1)

  let result_of_bool line b message =
    match b with
    | true -> Ok ()
    | false -> Error (line.number, Lazy.force message)

  let correct_citation_count deduction line =
    let expected_number =
      match line.rule with
      | PI -> 0
      | JE | DI | UI | UG -> 1
      | CI | CE | JI | DE -> 2
      | NI | NE -> 3
    in
    let citation_count = Array.length line.citations in
    result_of_bool line (citation_count = expected_number)
      (lazy
        (Printf.sprintf
           "Expected %d citations but received %d"
           expected_number
           citation_count))

  let citations_exist deduction line =
    match Array.find line.citations ~f:(fun n -> n > Array.length deduction) with
    | None -> Ok ()
    | Some cited ->
      Error (line.number, Printf.sprintf "Citation of nonexistent line %d" cited)

  let validate_citations deduction line =
    let open Result.Monad_infix in
    correct_citation_count deduction line >>= fun () ->
    citations_exist deduction line

  let check_assumptions expected line =
    result_of_bool
      line
      ([%compare.equal: Set.M(Int).t] expected line.assumptions)
      (lazy "Incorrect assumptions")

  let correct deduction line =
    let open Result.Monad_infix in
    let result_of_bool = result_of_bool line in
    validate_citations deduction line >>= fun () ->
    match line.rule with
    | PI ->
      let expected_assumptions = Set.singleton (module Int) line.number in
      check_assumptions expected_assumptions line

    | CI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_assumptions =
        Set.union a.assumptions b.assumptions |> fun s -> Set.remove s a.number
      in
      check_assumptions expected_assumptions line >>= fun () ->
      (match line.expr with
       | Cond (ant, con) ->
         result_of_bool
           (Expression.equal ant a.expr)
           (lazy "First input isn't the antecedent.") >>= fun () ->
         result_of_bool
           (Expression.equal con b.expr)
           (lazy "Second input isn't the consequent.")
       | _ -> Error (line.number, "Output isn't a conditional."))

    | CE ->
      let a = get_line deduction line.citations.(0) in
      let if_a_then_line = get_line deduction line.citations.(1) in
      let expected_assumptions = Set.union a.assumptions if_a_then_line.assumptions in
      check_assumptions expected_assumptions line >>= fun () ->
      (match if_a_then_line.expr with
       | Cond (ant, con) ->
         result_of_bool
           (Expression.equal ant a.expr)
           (lazy "First input isn't the antecedent.") >>= fun () ->
         result_of_bool
           (Expression.equal con line.expr)
           (lazy "Output isn't the consequent.")
       | _ -> Error (line.number, "Second input isn't a conditional"))

    | NI ->
      let a = get_line deduction line.citations.(0) in
      let not_a = get_line deduction line.citations.(1) in
      let c = get_line deduction line.citations.(2) in
      let expected_assumptions =
        Set.union_list (module Int) [a.assumptions; not_a.assumptions; c.assumptions]
        |> fun s -> Set.remove s c.number
      in
      check_assumptions expected_assumptions line >>= fun () ->
      result_of_bool
        (Expression.equal not_a.expr @@ Expression.Neg a.expr)
        (lazy "Inputs are not negations of each other") >>= fun () ->
      result_of_bool
        (Expression.equal line.expr @@ Expression.Neg c.expr)
        (lazy "Output is not the negation of the last input")

    | NE ->
      let a = get_line deduction line.citations.(0) in
      let not_a = get_line deduction line.citations.(1) in
      let not_line = get_line deduction line.citations.(2) in
      let expected_assumptions =
        Set.union_list
          (module Int)
          [a.assumptions; not_a.assumptions; not_line.assumptions]
        |> fun s -> Set.remove s not_line.number
      in
      check_assumptions expected_assumptions line >>= fun () ->
      result_of_bool
        (Expression.equal not_a.expr @@ Expression.Neg a.expr)
        (lazy "Inputs are not negations of each other") >>= fun () ->
      result_of_bool
        (Expression.equal not_line.expr @@ Expression.Neg line.expr)
        (lazy "Last input is not the negation of the output")

    | JI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_assumptions = Set.union a.assumptions b.assumptions in
      check_assumptions expected_assumptions line >>= fun () ->
      result_of_bool
        (Expression.equal line.expr @@ Expression.Conj (a.expr, b.expr))
        (lazy "Output isn't the conjunction of the inputs")

    | JE ->
      let a = get_line deduction line.citations.(0) in
      check_assumptions a.assumptions line >>= fun () ->
      result_of_bool
        (match a.expr with
         | Conj (x, y) -> Expression.equal line.expr x
                          || Expression.equal line.expr y
         | _ -> false)
        (lazy "Neither conjunct matches the line")

    | DI ->
      let a = get_line deduction line.citations.(0) in
      check_assumptions a.assumptions line >>= fun () ->
      result_of_bool
        (match line.expr with
         | Disj (x, y) -> Expression.equal a.expr x
                          || Expression.equal a.expr y
         | _ -> false)
        (lazy "Neither disjunct matches the line")

    | DE ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_assumptions = Set.union a.assumptions b.assumptions in
      check_assumptions expected_assumptions line >>= fun () ->
      result_of_bool
        (match a.expr with
         | Disj (x, y) -> Expression.equal b.expr (Expression.Neg x)
                          || Expression.equal b.expr (Expression.Neg y)
         | _ -> false)
        (lazy "Neither disjunct is negated")

    | UI ->
      let a = get_line deduction line.citations.(0) in
      check_assumptions a.assumptions line >>= fun () ->
      result_of_bool
        (Fol.is_instance a.expr line.expr)
        (lazy "That isn't an instance")

    | UG ->
      let a = get_line deduction line.citations.(0) in
      check_assumptions a.assumptions line >>= fun () ->
      (match line.expr with
       | Forall (v, e) ->
         result_of_bool
           (Expression.equal e a.expr)
           (lazy "Generalization doesn't match the premise") >>= fun () ->
         let earlier_lines = Set.to_list a.assumptions
                             |> List.map ~f:(fun n ->
                                 Array.get deduction (n - 1))
         in
         let free_variables = List.concat_map earlier_lines ~f:(
             fun line -> Fol.free_variables line.expr) in
         result_of_bool
           (not @@ List.mem ~equal:Char.equal free_variables v)
           (lazy "Variable appears free in premise")
       | _ -> Error (line.number, "This isn't a universal generalization"))
end

type t = Line.t array

let validate t =
  Array.to_list t
  |> List.map ~f:(Line.correct t)
  |> Result.combine_errors_unit
