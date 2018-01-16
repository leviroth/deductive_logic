open Base
open Lwt
open Cohttp
open Cohttp_lwt_unix

module Json_adapter = struct
  module Input = struct
    type line = { assumptions : int list;
                  number : int;
                  expr : string;
                  citations : int array;
                  rule : Deductive_logic.Deduction.rule; } [@@deriving yojson]

    type t = line list [@@deriving yojson]

    let parse s =
      let open Deductive_logic in
      let lexbuf = Lexing.from_string s in
      Option.try_with (fun () -> Parser.expr_only Lexer.read lexbuf)

    let to_deduction_line { assumptions; number; expr; citations; rule; } =
      let open Deductive_logic in
      match parse expr with
      | Some expr -> Ok (
        Deduction.Line.{
          assumptions = Set.of_list (module Int) assumptions;
          number;
          expr;
          citations;
          rule;
        })
      | None -> Result.failf "Couldn't parse line %d" number

    let to_deduction l =
      List.map l to_deduction_line
      |> Result.all
      |> Result.map ~f:Array.of_list
  end

  type feedback = (int * string) list [@@deriving yojson]
end

let build_deduction s =
  let open Result in
  try_with (fun () -> Yojson.Safe.from_string s)
  |> map_error ~f:(fun _ -> "Invalid json") >>=
  Json_adapter.Input.of_yojson >>=
  Json_adapter.Input.to_deduction

let string_of_result =
  function
  | Ok () -> "[]"
  | Error l -> Json_adapter.feedback_to_yojson l |> Yojson.Safe.to_string

let response input =
  match build_deduction input with
  | Ok deduction ->
    deduction
    |> Deductive_logic.Deduction.validate
    |> string_of_result
  | Error s -> s

let server =
  let callback _conn req body =
    body |> Cohttp_lwt.Body.to_string >|= response
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
