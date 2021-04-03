open Pervasives

(* [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/interp/ast-interp-resources/" filename
;;

(* Interpreting the program at [filepath_no_suffix.soml],
 * redirect output to [filepath_no_suffix.actual],
 * check output against [filepath_no_suffix.expect] *)
let _check_ast_interp (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let expect_path = String.append filepath_no_suffix ".expect" in
  let actual_path = String.append filepath_no_suffix ".actual" in
  let ast = Test_aux.parse_ast_exn filepath in

  (* Temporarily redirect stdout to [actual_path] *)
  let oldstdout = Unix.dup Unix.stdout in
  let newstdout = open_out actual_path in
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;
  match Ast_interp.interp_struct ast with
  | Ok _ -> ()
  | Error err -> Io.println (Frontend_pp.pp_ast_interp_error err);
  flush stdout;
  Unix.dup2 oldstdout Unix.stdout; (* redirct stdout back *)
  Stdlib.close_out newstdout;

  let expect = Externals.read_entire_file expect_path in
  let actual = Externals.read_entire_file actual_path in
  OUnit2.assert_equal expect actual;
;;


let tests = OUnit2.(>:::) "ast_interp_test" [

    OUnit2.(>::) "test_integration" (fun _ ->
        _check_ast_interp (_get_full_path "fac");
        _check_ast_interp (_get_full_path "short_circuit");
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
