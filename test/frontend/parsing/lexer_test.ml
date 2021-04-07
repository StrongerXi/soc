open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/parsing/lexer-resources/" filename
;;

let _check_lexer_pp_ast (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let result =
    match Driver.lex_file filepath with
    | Error msg -> msg
    | Ok ast -> String.join_with (List.map Pretty.pp_token ast) "\n"
  in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;

let tests = OUnit2.(>:::) "lexer_test" [

    OUnit2.(>::) "test_blank_source" (fun _ ->
        OUnit2.assert_equal (Ok []) (Lexer.lex "");
        OUnit2.assert_equal (Ok []) (Lexer.lex "\t\t\n   \n");
      );

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "lexer_input_mixed" in
        _check_lexer_pp_ast path
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
