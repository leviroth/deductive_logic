open Base

type rule = PI | CI | CE | NI | NE | JI | JE | DI | DE | UI | UG [@@deriving compare]

module Line = struct
  type t = { premises : Set.M(Int).t;
             number : int;
             expr : Expression.t;
             citations : int array;
             rule : rule; } [@@deriving compare]

  let get_line deduction n = deduction.(n - 1)

  let correct_citation_count deduction line =
    let expected_number =
      match line.rule with
      | PI -> 0
      | JE | DI | UI | UG -> 1
      | CI | CE | JI | DE -> 2
      | NI | NE -> 3
    in
    let citation_count = Array.length line.citations in
    if citation_count = expected_number
    then Ok ()
    else Error (line.number,
                Printf.sprintf
                  "Expected %d citations but received %d"
                  expected_number
                  citation_count)

  let citations_exist deduction line =
    match Array.find line.citations ~f:(fun n -> n > Array.length deduction) with
    | None -> Ok ()
    | Some cited ->
      Error (line.number, Printf.sprintf "Citation of nonexistent line %d" cited)

  let validate_citations deduction line =
    let open Result.Monad_infix in
    correct_citation_count deduction line >>= fun () ->
    citations_exist deduction line

  let result_of_bool line b message =
    match b with
    | true -> Ok ()
    | false -> Error (line.number, Lazy.force message)

  let check_premises expected line =
    result_of_bool
      line
      ([%compare.equal: Set.M(Int).t] expected line.premises)
      (lazy (Printf.sprintf "Incorrect premises on line %d" line.number))

  let correct deduction line =
    let open Result.Monad_infix in
    let result_of_bool = result_of_bool line in
    validate_citations deduction line >>= fun () ->
    match line.rule with
    | PI ->
      let expected_premises = Set.singleton (module Int) line.number in
      check_premises expected_premises line

    | CI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises =
        Set.union a.premises b.premises |> fun s -> Set.remove s a.number
      in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (Expression.equal line.expr @@ Expression.Cond (a.expr, b.expr))
        (lazy "Line does not match premises")

    | CE ->
      let a = get_line deduction line.citations.(0) in
      let if_a_then_line = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises if_a_then_line.premises in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (Expression.equal if_a_then_line.expr @@ Expression.Cond (a.expr, line.expr))
        (lazy "Line does not match premises")

    | NI ->
      let a = get_line deduction line.citations.(0) in
      let not_a = get_line deduction line.citations.(1) in
      let c = get_line deduction line.citations.(2) in
      let expected_premises =
        Set.union_list (module Int) [a.premises; not_a.premises; c.premises]
        |> fun s -> Set.remove s c.number
      in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (Expression.equal not_a.expr @@ Expression.Neg a.expr)
        (lazy "Premises are not negations of each other") >>= fun () ->
      result_of_bool
        (Expression.equal line.expr @@ Expression.Neg c.expr)
        (lazy "Line is not the negation of the premise")

    | NE ->
      let a = get_line deduction line.citations.(0) in
      let not_a = get_line deduction line.citations.(1) in
      let not_line = get_line deduction line.citations.(2) in
      let expected_premises =
        Set.union_list
          (module Int)
          [a.premises; not_a.premises; not_line.premises]
        |> fun s -> Set.remove s not_line.number
      in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (Expression.equal not_a.expr @@ Expression.Neg a.expr)
        (lazy "Premises are not negations of each other") >>= fun () ->
      result_of_bool
        (Expression.equal not_line.expr @@ Expression.Neg line.expr)
        (lazy "Premise is not the negation of the line")

    | JI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises b.premises in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (Expression.equal line.expr @@ Expression.Conj (a.expr, b.expr))
        (lazy "This isn't the conjunction of the premises")

    | JE ->
      let a = get_line deduction line.citations.(0) in
      check_premises a.premises line >>= fun () ->
      result_of_bool
        (match a.expr with
         | Conj (x, y) -> Expression.equal line.expr x
                          || Expression.equal line.expr y
         | _ -> false)
        (lazy "Neither conjunct matches the line")

    | DI ->
      let a = get_line deduction line.citations.(0) in
      check_premises a.premises line >>= fun () ->
      result_of_bool
        (match line.expr with
         | Disj (x, y) -> Expression.equal a.expr x
                          || Expression.equal a.expr y
         | _ -> false)
        (lazy "Neither disjunct matches the line")

    | DE ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises b.premises in
      check_premises expected_premises line >>= fun () ->
      result_of_bool
        (match a.expr with
         | Disj (x, y) -> Expression.equal b.expr (Expression.Neg x)
                          || Expression.equal b.expr (Expression.Neg y)
         | _ -> false)
        (lazy "Neither disjunct is negated")

    | UI ->
      let a = get_line deduction line.citations.(0) in
      check_premises a.premises line >>= fun () ->
      result_of_bool
        (Fol.is_instance a.expr line.expr)
        (lazy "That isn't an instance")

    | UG ->
      let a = get_line deduction line.citations.(0) in
      check_premises a.premises line >>= fun () ->
      (match line.expr with
       | Forall (v, e) ->
         result_of_bool
           (Expression.equal e a.expr)
           (lazy "Generalization doesn't match the premise") >>= fun () ->
         let earlier_lines = Set.to_list a.premises
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
  Array.fold_result t ~init:() ~f:(fun () l -> Line.correct t l)
