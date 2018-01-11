open Base
open Stdio
open Deductive_logic

let deduction_of_ic ic =
  let lexbuf = Lexing.from_channel ic in
  Parser.deduction Lexer.read lexbuf

let deduction_of_filename filename =
  In_channel.with_file filename ~f:deduction_of_ic

let string_of_result =
  function
  | Ok () -> "Deduction is correct"
  | Error (i, m) -> Printf.sprintf "Error on line %d: %s" i m

let () =
  let deduction =
    match Caml.Sys.argv.(1) with
    | s -> deduction_of_filename s
    | exception Invalid_argument _ -> deduction_of_ic stdin
  in
  Deduction.validate deduction
  |> string_of_result
  |> printf "%s\n"
