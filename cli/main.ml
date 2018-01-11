open Base
open Stdio
open Deductive_logic

let construct_deduction filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      Parser.deduction Lexer.read lexbuf)

let string_of_result =
  function
  | Ok () -> "Deduction is correct"
  | Error (i, m) -> Printf.sprintf "Error on line %d: %s\n" i m

let () =
  let filename = Caml.Sys.argv.(1) in
  construct_deduction filename
  |> Deduction.validate
  |> string_of_result
  |> printf "%s\n"
