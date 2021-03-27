
let tests = OUnit2.(>:::) "location_test" [

    OUnit2.(>::) "test_to_string" (fun _ ->
        OUnit2.assert_equal "(3, 7)" (Location.to_string (Location.create 3 7));
        OUnit2.assert_equal "(0, 0)" (Location.to_string (Location.create 0 0));
      );

    OUnit2.(>::) "test_advance" (fun _ ->
        OUnit2.assert_equal
          (Location.create 0 1)
          (Location.advance (Location.create 0 0));
        OUnit2.assert_equal
          (Location.create 3 3)
          (Location.advance (Location.create 3 2));
      );

    OUnit2.(>::) "test_skip_line" (fun _ ->
        OUnit2.assert_equal
          (Location.create 1 0)
          (Location.skip_line (Location.create 0 0));
        OUnit2.assert_equal
          (Location.create 1 0)
          (Location.skip_line (Location.create 0 2000));
        OUnit2.assert_equal
          (Location.create 11 0)
          (Location.skip_line (Location.create 10 0));
        OUnit2.assert_equal
          (Location.create 11 0)
          (Location.skip_line (Location.create 10 21));
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
