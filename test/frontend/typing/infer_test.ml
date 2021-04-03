open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/typing/resources/" filename
;;

let _check_infer_struct (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let ast = Test_aux.parse_ast_exn filepath in
  let result =
    match Typer.type_struct ast with
    | Error errs ->
      String.join_with (List.map Frontend_pp.pp_typer_error errs) "\n"
    | Ok ast ->
      Frontend_pp.pp_ast_structure ast
  in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "infer_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "infer_input_mixed" in
        _check_infer_struct path;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
