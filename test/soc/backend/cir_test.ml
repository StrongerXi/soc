open Pervasives

(* [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  "../../../../test/soc/backend/cir-resources/" ^ filename
;;

let _check_cir_pp (filepath_no_suffix : string) : unit =
  let filepath = filepath_no_suffix ^ ".soml" in
  let result =
    match Driver.cir_file filepath with
    | Error msg -> msg
    | Ok cir -> Pretty.pp_cir cir
  in
  let expect_path = filepath_no_suffix ^ ".expect" in
  let actual_path = filepath_no_suffix ^ ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "cir_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        _check_cir_pp (_get_full_path "freevar")
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
