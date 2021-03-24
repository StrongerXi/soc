open Pervasives

let tests = OUnit2.(>:::) "option_test" [
    OUnit2.(>::) "test_some" (fun _ ->
        OUnit2.assert_equal (Some 42) (Option.some 42);
      );

    OUnit2.(>::) "test_value" (fun _ ->
        OUnit2.assert_equal 42 (Option.value None      42);
        OUnit2.assert_equal 11 (Option.value (Some 11) 42);
      );

    OUnit2.(>::) "test_bind" (fun _ ->
        let is_42 n = if n = 42 then Some n else None in
        OUnit2.assert_equal None      (Option.bind None is_42);
        OUnit2.assert_equal None      (Option.bind (Some 11) is_42);
        OUnit2.assert_equal (Some 42) (Option.bind (Some 42) is_42);
      );

    OUnit2.(>::) "test_join" (fun _ ->
        OUnit2.assert_equal None      (Option.join None);
        OUnit2.assert_equal None      (Option.join (Some None));
        OUnit2.assert_equal (Some 42) (Option.join (Some (Some 42)));
      );

    OUnit2.(>::) "test_map" (fun _ ->
        let add1 = fun n -> n + 1 in
        OUnit2.assert_equal None      (Option.map add1 None);
        OUnit2.assert_equal (Some 43) (Option.map add1 (Some 42));
      );

    OUnit2.(>::) "test_iter" (fun _ ->
        let num_cell = ref 42 in
        let add n = num_cell := !num_cell + n in
        OUnit2.assert_equal () (Option.iter add None);
        OUnit2.assert_equal 42 !num_cell;
        OUnit2.assert_equal () (Option.iter add (Some 10));
        OUnit2.assert_equal 52 !num_cell;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
