open Pervasives

let tests = OUnit2.(>:::) "int_test" [
    OUnit2.(>::) "test_max" (fun _ ->
        OUnit2.assert_equal 5 (Int.max 5 5);
        OUnit2.assert_equal 7 (Int.max 3 7);
        OUnit2.assert_equal ~-2 (Int.max ~-2 ~-4);
      );

    OUnit2.(>::) "test_min" (fun _ ->
        OUnit2.assert_equal 5 (Int.min 5 5);
        OUnit2.assert_equal 3 (Int.min 3 7);
        OUnit2.assert_equal ~-4 (Int.min ~-2 ~-4);
      );

    OUnit2.(>::) "test_compare" (fun _ ->
        OUnit2.assert_equal true ((Int.compare 5 5) = 0);
        OUnit2.assert_equal true ((Int.compare 3 7) < 0);
        OUnit2.assert_equal true ((Int.compare ~-2 ~-4) > 0);
      );

    OUnit2.(>::) "test_to_string" (fun _ ->
        OUnit2.assert_equal "42" (Int.to_string 42);
        OUnit2.assert_equal "0" (Int.to_string 0);
        OUnit2.assert_equal "-13" (Int.to_string ~-13);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
