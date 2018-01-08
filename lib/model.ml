open Base

type t = bool Map.M(Char).t

let empty = Map.empty (module Char)

let of_alist_exn l = Map.of_alist_exn (module Char) l

let eval t expr =
  let rec eval expr =
    let open Expression in
    match expr with
    | Prop p -> Map.find t p
    | Neg e -> Option.map (eval e) ~f:not
    | Cond (e1, e2) -> Option.map2 (eval e1) (eval e2) ~f:(fun hyp con ->
        not hyp || con)
    | Conj (e1, e2) -> Option.map2 (eval e1) (eval e2) ~f:(&&)
    | Disj (e1, e2) -> Option.map2 (eval e1) (eval e2) ~f:(||)
  in eval expr

let letters_used expr =
  let rec aux expr acc =
    let open Expression in
    match expr with
    | Prop p -> Set.add acc p
    | Neg e -> aux e acc
    | Cond (e1, e2) | Conj (e1, e2) | Disj (e1, e2)
      -> let acc' = (aux e1 acc) in aux e2 acc'
  in
  aux expr @@ Set.empty (module Char)

let all expressions =
  let rec aux (letters : char list) (acc : t list) =
    match letters with
    | [] -> acc
    | hd :: tl ->
      let products = List.cartesian_product acc @@ [hd, true; hd, false] in
      let acc = List.map products ~f:(fun (model, (key, data)) ->
          Map.set model ~key ~data)
      in
      aux tl acc
  in
  let letters =
    List.map expressions ~f:letters_used
    |> Set.union_list (module Char)
    |> Set.to_list
  in
  aux letters [Map.empty (module Char)]
