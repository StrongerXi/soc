open Pervasives

let tests = OUnit2.(>:::) "temp_test" [

    (* NOTE as long as [to_string] uniquely distinguishes temps, we don't care
     * what it actually returns *)
    OUnit2.(>::) "test_gen" (fun _ ->
        let manager = Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in
        let _, t3 = Temp.gen manager in
        OUnit2.assert_equal false ((Temp.to_string t1) = (Temp.to_string t2));
        OUnit2.assert_equal false ((Temp.to_string t2) = (Temp.to_string t3));
        OUnit2.assert_equal false ((Temp.to_string t3) = (Temp.to_string t1));
      );

    (* test with [get] together *)
    OUnit2.(>::) "test_gen_and_bind" (fun _ ->
        let manager = Temp.init_manager in
        let s1, s2 = "s1", "s2" in
        let manager, t1 = Temp.gen_and_bind manager s1 in
        let manager, t2 = Temp.gen_and_bind manager s2 in
        let manager, t3 = Temp.gen manager in
        let manager, t4 = Temp.gen manager in
        OUnit2.assert_equal false ((Temp.to_string t1) = (Temp.to_string t2));
        OUnit2.assert_equal false ((Temp.to_string t2) = (Temp.to_string t3));
        OUnit2.assert_equal false ((Temp.to_string t3) = (Temp.to_string t4));
        OUnit2.assert_equal false ((Temp.to_string t4) = (Temp.to_string t1));
        OUnit2.assert_equal false ((Temp.to_string t3) = (Temp.to_string t2));
        OUnit2.assert_equal false ((Temp.to_string t4) = (Temp.to_string t2));
        let t1' = Temp.get manager s1 in
        let t2' = Temp.get manager s2 in
        OUnit2.assert_equal
          (Some (Temp.to_string t1))
          (Option.map Temp.to_string t1');
        OUnit2.assert_equal
          (Some (Temp.to_string t2))
          (Option.map Temp.to_string t2');
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
