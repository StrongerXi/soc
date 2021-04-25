
let _soc_path = "../../../../soc"
;;

(* NOTE must close output fd *)
let _get_empty_stdin () : Unix.file_descr =
  let stdin_read, stdin_write = Unix.pipe () in
  Unix.close stdin_write;
  stdin_read
;;

let _check_files_match (ref_path : string) (output_path : string) : unit =
  let ref_str = Externals.read_entire_file ref_path in
  let output_str = Externals.read_entire_file output_path in
  OUnit2.assert_equal ref_str output_str;
;;

(* Launch [prog] with [args] and wait till it finishes.
 * Return error as strings, if any *)
let _run_process_collect_error
    (prog : string) (args : string array)
  : (unit, string * string) result = (* Error (stdout_str, stderr_str) *)

  (* generate temporary file names *)
  let stdout_path = Stdlib.Filename.temp_file "ignore_success_tmp" ".stdout" in
  let stderr_path = Stdlib.Filename.temp_file "ignore_success_tmp" ".stderr" in

  (* open resources for IO *)
  let stdin_fd = _get_empty_stdin () in
  let stdout_oc = open_out stdout_path in
  let stderr_oc = open_out stderr_path in

  (* launch and finish process *)
  let pid =
    Unix.create_process prog args stdin_fd
      (Unix.descr_of_out_channel stdout_oc)
      (Unix.descr_of_out_channel stderr_oc)
  in
  let (_, status) = Unix.waitpid [] pid in

  (* close resources for IO *)
  Unix.close stdin_fd;
  close_out stdout_oc;
  close_out stderr_oc;

  match status with
  | WEXITED 0 -> Ok ()
  | _ ->
    let stdout_str = Externals.read_entire_file stdout_path in
    let stderr_str = Externals.read_entire_file stderr_path in
    Error (stdout_str, stderr_str)
;;

(* Execute [exe_path], direct all output to a new file at [output_path], and
 * compare it with content of file at [ref_output_path] *)
let _exec_write_and_check_output
    (exe_path : string) (ref_output_path : string) (output_path : string)
  : unit =
  let output_oc = open_out output_path in
  let output_fd = Unix.descr_of_out_channel output_oc in
  (* NOTE to simulate stdin input in the future, just use pipe and write
   * "input" to the write end here *)
  let exe_stdin = _get_empty_stdin () in
  let exe_pid =
    Unix.create_process exe_path ([|exe_path|]) exe_stdin output_fd output_fd
  in
  let _, exe_status = Unix.waitpid [] exe_pid in
  let () =
    match exe_status with
    | WEXITED _ -> () (* error, if any, was already written to output *)
    | WSIGNALED n ->
      output_string output_oc
        (Printf.sprintf "Process killed by signal %d" n)
    | WSTOPPED n ->
      output_string output_oc
        (Printf.sprintf "Process stopped by signal %d" n)
  in
  Unix.close exe_stdin;
  close_out output_oc; (* this closes the file descriptor too *)
  _check_files_match ref_output_path output_path;
;;


(* Compile "[test_name].soml", run the executable, and check output *)
let _run_test (test_name : string) : unit =
  let path_prefix = "../../../../test/integration/resources/" ^ test_name in
  let source_path = path_prefix ^ ".soml" in
  let exe_path = path_prefix ^ ".exe" in
  let ref_output_path = path_prefix ^ ".expect" in
  let output_path = path_prefix ^ ".actual" in

  match _run_process_collect_error _soc_path ([|_soc_path; source_path|]) 
  with
  | Ok () ->
    _exec_write_and_check_output exe_path ref_output_path output_path

  | Error (stdout_str, stderr_str) ->
    let output = "-------STDOUT:\n" ^ stdout_str ^ "\n" ^
                 "-------STDERR:\n" ^ stderr_str
    in
    Externals.write_entire_file output_path output;
    OUnit2.assert_failure ("Compilation failed for " ^ source_path);
;;

let _create_ounit_test (test_name : string) : OUnit2.test =
  OUnit2.(>::)
    ("test_" ^ test_name)
    (fun _ -> _run_test test_name)
;;


(* NOTE to add an integration test, just
   0. Add the test name [X] in the list below.
   1. Add test source code [X.soml] in resources folder.
   2. Add expected output [X.expect] in resources folder. *)
let tests = OUnit2.(>:::) "test_integration"
    (Stdlib.List.map _create_ounit_test
       [ 
         "basic";
       ])

let _ =
  OUnit2.run_test_tt_main tests
