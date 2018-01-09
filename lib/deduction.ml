open Base

type rule = PI | CI | CE | NI | NE | JI | JE | DI | DE [@@deriving compare]

module Line = struct
  type t = { premises : Set.M(Int).t;
             number : int;
             expr : Expression.t;
             citations : int array;
             rule : rule; } [@@deriving compare]

  let get_line deduction n = deduction.(n - 1)

  let correct deduction line =
    match line.rule with
    | PI ->
      let expected_premises = Set.singleton (module Int) line.number in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises

    | CI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises =
        Set.union a.premises b.premises |> fun s -> Set.remove s a.number in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && Expression.equal line.expr @@ Expression.Cond (a.expr, b.expr)

    | CE ->
      let a = get_line deduction line.citations.(0) in
      let if_a_then_line = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises if_a_then_line.premises in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && Expression.equal if_a_then_line.expr @@ Expression.Cond (a.expr, line.expr)

    | NI ->
      let a = get_line deduction line.citations.(0) in
      let not_a = get_line deduction line.citations.(1) in
      let c = get_line deduction line.citations.(2) in
      let expected_premises =
        Set.union_list (module Int) [a.premises; not_a.premises; c.premises]
        |> fun s -> Set.remove s c.number
      in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && Expression.equal not_a.expr @@ Expression.Neg a.expr
      && Expression.equal line.expr @@ Expression.Neg c.expr

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
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && Expression.equal not_a.expr @@ Expression.Neg a.expr
      && Expression.equal not_line.expr @@ Expression.Neg line.expr

    | JI ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises b.premises in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && Expression.equal line.expr @@ Expression.Conj (a.expr, b.expr)

    | JE ->
      let a = get_line deduction line.citations.(0) in
      [%compare.equal: Set.M(Int).t] line.premises a.premises
      && (match a.expr with
          | Conj (x, y) -> Expression.equal line.expr x
                           || Expression.equal line.expr y
          | _ -> false)

    | DI ->
      let a = get_line deduction line.citations.(0) in
      [%compare.equal: Set.M(Int).t] line.premises a.premises
      && (match line.expr with
          | Disj (x, y) -> Expression.equal a.expr x
                           || Expression.equal a.expr y
          | _ -> false)

    | DE ->
      let a = get_line deduction line.citations.(0) in
      let b = get_line deduction line.citations.(1) in
      let expected_premises = Set.union a.premises b.premises in
      [%compare.equal: Set.M(Int).t] line.premises expected_premises
      && (match a.expr with
          | Disj (x, y) -> Expression.equal b.expr (Expression.Neg x)
                           || Expression.equal b.expr (Expression.Neg y)
          | _ -> false)
end

type t = Line.t array

let valid t = Array.for_all t ~f:(Line.correct t)
