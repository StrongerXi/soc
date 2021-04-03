open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/typing/resources/" filename
;;

let _parse_ast_exn (filepath : string) : Ast.structure =
  let lexer = Lexer.create filepath in
  match Parser.parse lexer with
  | Error err ->
    let msg = ("Unexpected parsing failure on [" ^ filepath ^ "]:\n") in
    let msg = String.append msg (Frontend_pp.pp_parser_error err) in
    OUnit2.assert_failure msg;
  | Ok ast -> ast
;;

let _check_infer_struct (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let ast = _parse_ast_exn filepath in
  let result =
    match Infer.infer_struct ast with
    | Error errs ->
      String.join_with (List.map Frontend_pp.pp_infer_error errs) "\n"
    | Ok ast ->
      Frontend_pp.pp_ast_structure ast
  in
  let output = Stdlib.open_out (String.append filepath_no_suffix ".actual") in
  Stdlib.output_string output result;
  Stdlib.close_out output;
  let expect = Externals.read_entire_file (String.append filepath_no_suffix ".expect") in
  OUnit2.assert_equal expect result;
;;


let tests = OUnit2.(>:::) "infer_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "infer_input_mixed" in
        _check_infer_struct path;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
