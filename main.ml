open Base
open Stdio

let parse lexbuf = Parser.line Lexer.read lexbuf

let () =
  "-(p & q)"
  |> Lexing.from_string
  |> parse
  |> Expression.sexp_of_t
  |> Sexp.to_string
  |> printf "%s\n"
