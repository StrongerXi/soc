open Pervasives

let () =
  let filename = Sys.argv.(1) in
  match Driver.type_file filename with
  | Ok ast -> Io.println (Pretty.pp_ast_structure ast)
  | Error msg -> Io.println msg
