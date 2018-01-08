open Base

type rule = PI | CI | CE | NI | NE | JI | JE | DI | DE [@@deriving compare]

module Line = struct
  type t = { premises : Set.M(Int).t;
             number : int;
             expr : Expression.t;
             citations : int array;
             rule : rule; } [@@deriving compare]
end

type t = Line.t array
