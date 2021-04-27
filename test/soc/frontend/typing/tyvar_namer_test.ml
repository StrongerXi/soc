open Pervasives

(* [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../../test/soc/frontend/typing/tyvar-namer-resources/" filename
;;

let _check_renamed_ast_pp (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let result =
    match Driver.parse_file filepath with
    | Error msg -> msg
    | Ok ast -> 
      let namer = Tyvar_namer.init in
      let _, ast = Tyvar_namer.rename_struct namer ast in
      Pretty.pp_ast_structure ast
  in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "tyvar_namer_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        List.iter
          (fun name -> _check_renamed_ast_pp (_get_full_path name))
          [
            "tyvars";
          ]
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
