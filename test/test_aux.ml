
(** [check_and_output_str str ref_path output_path] checks whether [str] matches
    the content of the file at [ref_path], and (over)writes [str] to
    [output_path] *)
let check_and_output_str str ref_path output_path =
  let output = Stdlib.open_out output_path in
  Stdlib.output_string output str;
  Stdlib.close_out output;
  let ref_str = Externals.read_entire_file ref_path in
  OUnit2.assert_equal ref_str str;
;;


(** [parse_ast_exn filepath] parses the file at [filepath] and return the AST.
    It fails upon any parser or lexer failure *)
let parse_ast_exn (filepath : string) : Ast.structure =
  let lexer = Lexer.create filepath in
  match Parser.parse lexer with
  | Error err ->
    let msg = ("Unexpected parsing failure on [" ^ filepath ^ "]:\n") in
    let msg = String.append msg (Frontend_pp.pp_parser_error err) in
    OUnit2.assert_failure msg;
  | Ok ast -> ast
;;
