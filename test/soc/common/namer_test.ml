
open Pervasives

(* [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  "../../../../test/soc/common/namer-resources/" ^ filename
;;

let _check_ast_with_renamer
    (filepath_no_suffix : string) (renamer : Ast.structure -> Ast.structure)
  : unit =
  let filepath = filepath_no_suffix ^ ".soml" in
  let result =
    match Driver.parse_file filepath with
    | Error msg -> msg
    | Ok ast -> Pretty.pp_ast_structure (renamer ast)
  in
  let expect_path = filepath_no_suffix ^ ".expect" in
  let actual_path = filepath_no_suffix ^ ".actual" in
  Test_aux.check_and_output_str result expect_path actual_path;
;;


let tests = OUnit2.(>:::) "namer_test" [

    OUnit2.(>::) "test_rename_tyvars_in_ast_struct" (fun _ ->
        List.iter
          (fun name ->
             let renamer ast =
               let namer = Namer.init "X" in
               Namer.rename_tyvars_in_ast_struct namer ast
               |> (fun (_, renamed_ast) -> renamed_ast)
             in
             _check_ast_with_renamer (_get_full_path name) renamer)
          [
            "tyvars";
          ]
      );

    OUnit2.(>::) "test_rename_vars_in_ast_struct" (fun _ ->
        List.iter
          (fun name ->
             let renamer ast =
               let namer = Namer.init "x" in
               Namer.rename_vars_in_ast_struct namer ast
               |> (fun (_, renamed_ast) -> renamed_ast)
             in
             _check_ast_with_renamer (_get_full_path name) renamer)
          [
            "vars_in_let";
          ]
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
