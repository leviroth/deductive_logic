open Base

type t = bool Map.M(Char).t

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
