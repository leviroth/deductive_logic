open Base

module T = struct
  type t =
    | Prop of char
    | Relation of char * char list
    | Neg of t
    | Cond of t * t
    | Conj of t * t
    | Disj of t * t
    | Forall of char * t
  [@@deriving sexp, compare]
end
include T
include Comparable.Make(T)
