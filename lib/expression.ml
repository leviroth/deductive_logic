open Base

type t =
  | Prop of char
  | Neg of t
  | Cond of t * t
  | Conj of t * t
  | Disj of t * t
[@@deriving sexp, compare]
