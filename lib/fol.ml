open Base

let create_instance general v =
  let open Expression in
  let rec create_instance formula u v bound =
    let subexpr e = create_instance e u v bound in
    if Set.mem bound u || Set.mem bound v then formula
    else match formula with
      | Prop c -> Prop c
      | Relation (f, l) ->
        Relation (f, List.map l ~f:(fun c -> if Char.equal c u then v else c))
      | Neg e -> subexpr e
      | Cond (e1, e2) -> Cond (subexpr e1, subexpr e2)
      | Conj (e1, e2) -> Conj (subexpr e1, subexpr e2)
      | Disj (e1, e2) -> Disj (subexpr e1, subexpr e2)
      | Forall (w, e) -> Forall (w, create_instance e u v (Set.add bound w))
  in
  match general with
  | Forall (u, e) -> create_instance e u v (Set.empty (module Char))
  | _ -> failwith "Not a universal generalization"

let free_variables formula =
  let open Expression in
  let rec free_variables formula bound acc =
    match formula with
      | Prop c -> acc
      | Relation (f, l) -> List.filter l ~f:(fun v -> not @@ Set.mem bound v) @ acc
      | Neg e -> free_variables e bound acc
      | Cond (e1, e2) | Conj (e1, e2) | Disj (e1, e2)
        -> let acc' = free_variables e1 bound acc in free_variables e2 bound acc'
      | Forall (v, e) -> free_variables e (Set.add bound v) acc
  in
  free_variables formula (Set.empty (module Char)) []
  |> Set.of_list (module Char)
  |> Set.to_list

let is_instance big small =
  let open Expression in
  match big with
  | Forall (v, phi) ->
    equal phi small
    || List.exists (free_variables small) ~f:(fun v ->
        equal small @@ create_instance big v)
  | _ -> false
