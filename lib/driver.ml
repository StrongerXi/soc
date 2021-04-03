open Pervasives

(* TODO
 * - add optional trace info (string output for each completed stage)
 * - add config (that's probably much later) *)

let parse_file filepath =
  let lexer = Lexer.create filepath in
  try 
    match Parser.parse lexer with
    | Error err -> Error (Frontend_pp.pp_parser_error err)
    | Ok ast -> Ok ast
  with Lexer.Lexer_error err -> Error (Frontend_pp.pp_lexer_error err)
;;

let type_file filepath =
  let type_ast ast =
    match Typer.type_struct ast with
    | Error errs ->
      Error (String.join_with (List.map Frontend_pp.pp_typer_error errs) "\n")
    | Ok ast -> Ok ast
  in
  Result.bind (parse_file filepath) type_ast
