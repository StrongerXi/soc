open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/soc/frontend/parsing/parser-resources/" filename
;;

let _check_parser_pp_ast (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let result =
    match Driver.parse_file filepath with
    | Error msg -> msg
    | Ok ast -> Pretty.pp_ast_structure ast
  in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "parser_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        _check_parser_pp_ast (_get_full_path "parser_input_mixed");
        _check_parser_pp_ast (_get_full_path "parser_input_syntax_sugar");
        _check_parser_pp_ast (_get_full_path "parser_input_infix");
        _check_parser_pp_ast (_get_full_path "eof");
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
