open Pervasives

let tests = OUnit2.(>:::) "pervasives_tests" [

    OUnit2.(>::) "test_not" (fun _ ->
        OUnit2.assert_equal true (not false);
        OUnit2.assert_equal false (not true);
      );

    (* sanity check on purity *)
    OUnit2.(>::) "test_pipeline" (fun _ ->
        let add1 x = x + 1 in
        OUnit2.assert_equal 1 (0 |> add1);
        OUnit2.assert_equal 4 (1 |> add1 |> add1 |> add1);
      );

    (* sanity check on purity *)
    OUnit2.(>::) "test_apply" (fun _ ->
        let add1 x = x + 1 in
        OUnit2.assert_equal 11 (add1 @@ 2 * 5);
        OUnit2.assert_equal 13 (add1 @@ 3 * 4);
      );

    OUnit2.(>::) "test_list_append" (fun _ ->
        OUnit2.assert_equal [] ([] @ []);
        OUnit2.assert_equal [1] ([] @ [1]);
        OUnit2.assert_equal [1] ([1] @ []);
        OUnit2.assert_equal [1; 2; 3; 4] ([1; 2] @ [3; 4]);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
