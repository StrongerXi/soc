open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/typing/resources/" filename
;;

let _check_typer_struct (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let result =
    match Driver.type_file filepath with
    | Error msg -> msg
    | Ok ast -> Frontend_pp.pp_ast_structure_with_typ_annot ast
  in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "typer_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        _check_typer_struct (_get_full_path "typer_input_mixed");
        _check_typer_struct (_get_full_path "annot");
        _check_typer_struct (_get_full_path "annot_error");
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
