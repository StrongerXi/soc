open Pervasives

(* TODO
 * - add optional trace info (string output for each completed stage)
 * - add config (that's probably much later) *)

let lex_file filepath =
  let content = Io.read_file filepath in
  match Lexer.lex content with
  | Error err -> Error (Pretty.pp_lexer_error err)
  | Ok tokens -> Ok tokens
;;

let parse_file filepath =
  let parse_tokens tokens =
    match Parser.parse tokens with
    | Error err -> Error (Pretty.pp_parser_error err)
    | Ok ast -> Ok ast
  in
  Result.bind (lex_file filepath) parse_tokens
;;

let type_file filepath =
  let type_ast ast =
    match Typer.type_struct ast with
    | Error errs ->
      Error (String.join_with (List.map Pretty.pp_typer_error errs) "\n")
    | Ok ast -> Ok ast
  in
  Result.bind (parse_file filepath) type_ast
;;

let cir_file filepath =
  Result.map Cir.from_ast_struct (type_file filepath)
;;
