open Pervasives

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/parser-resources/" filename
;;

let _check_parser_pp_ast (filepath_no_suffix : string) : unit =
  let filepath = String.append filepath_no_suffix ".soml" in
  let lexer = Lexer.create filepath in
  let stream = { Parser.next = (fun () -> Lexer.next lexer)
               ; where = (fun () -> Lexer.next_loc lexer) } in
  let result =
    match Parser.parse stream with
    | Error err -> Frontend_pp.pp_parser_error err
    | Ok ast -> Frontend_pp.pp_ast_structure ast
  in
  let output = Stdlib.open_out (String.append filepath_no_suffix ".actual") in
  Stdlib.output_string output result;
  Stdlib.close_out output;
  let expect = Externals.read_entire_file (String.append filepath_no_suffix ".expect") in
  OUnit2.assert_equal expect result;
;;


let tests = OUnit2.(>:::) "lexer_test" [

    OUnit2.(>::) "test_blank_file" (fun _ ->
        let stream = { Parser.next = (fun () -> None)
                     ; where = (fun () -> Location.create 0 0) } in
        let ast = Parser.parse stream in
        OUnit2.assert_equal (Ok []) ast
      );

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "parser_input_mixed" in
        _check_parser_pp_ast path;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests