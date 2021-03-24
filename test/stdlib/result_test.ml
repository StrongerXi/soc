open Pervasives

let tests = OUnit2.(>:::) "result_test" [
    OUnit2.(>::) "test_ok" (fun _ ->
        OUnit2.assert_equal (Ok 42) (Result.ok 42);
      );
    
    OUnit2.(>::) "test_error" (fun _ ->
        OUnit2.assert_equal (Error "abc") (Result.error "abc");
      );

    OUnit2.(>::) "test_bind" (fun _ ->
        let is_42 n = if n = 42 then Ok n else (Error 11) in
        OUnit2.assert_equal (Error 11)      (Result.bind (Error 11) is_42);
        OUnit2.assert_equal (Error 11)      (Result.bind (Ok 11) is_42);
        OUnit2.assert_equal (Ok 42) (Result.bind (Ok 42) is_42);
      );

    OUnit2.(>::) "test_join" (fun _ ->
        OUnit2.assert_equal (Error 11)      (Result.join (Error 11));
        OUnit2.assert_equal (Error 11)      (Result.join (Ok (Error 11)));
        OUnit2.assert_equal (Ok 42) (Result.join (Ok (Ok 42)));
      );

    OUnit2.(>::) "test_map" (fun _ ->
        let add1 = fun n -> n + 1 in
        OUnit2.assert_equal (Error 11)      (Result.map add1 (Error 11));
        OUnit2.assert_equal (Ok 43) (Result.map add1 (Ok 42));
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
