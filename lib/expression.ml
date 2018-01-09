open Base

module Atom = struct
  module T = struct
    type t =
      | Prop of char
      | Relation of char * char list
    [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

module T = struct
  type t =
    | Atom of Atom.t
    | Neg of t
    | Cond of t * t
    | Conj of t * t
    | Disj of t * t
    | Forall of char * t
  [@@deriving sexp, compare]
end
include T
include Comparable.Make(T)
