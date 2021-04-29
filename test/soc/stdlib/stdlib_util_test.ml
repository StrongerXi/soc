
let tests = OUnit2.(>:::) "stdlib_util_test" [

    OUnit2.(>::) "test_str_join_with" (fun _ ->
        OUnit2.assert_equal "" (Stdlib_util.str_join_with [] "");
        OUnit2.assert_equal "" (Stdlib_util.str_join_with [] "aaa");
        OUnit2.assert_equal "ss" (Stdlib_util.str_join_with ["ss"] "aaa");
        OUnit2.assert_equal "a; bb; a" (Stdlib_util.str_join_with ["a"; "bb"; "a"] "; ");
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
