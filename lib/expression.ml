open Base

module T = struct
  type t =
    | Prop of char
    | Neg of t
    | Cond of t * t
    | Conj of t * t
    | Disj of t * t
  [@@deriving sexp, compare]
end
include T
include Comparable.Make(T)
