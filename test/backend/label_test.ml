
open Pervasives

let tests = OUnit2.(>:::) "label_test" [

    (* NOTE as long as [to_string] uniquely distinguishes labels, we don't care
     * what it actually returns *)
    OUnit2.(>::) "test_gen" (fun _ ->
        let manager = Label.init_manager in
        let manager, t1 = Label.gen manager "s1" in
        let manager, t2 = Label.gen manager "s1" in
        let _, t3 = Label.gen manager "s2" in
        OUnit2.assert_equal false ((Label.to_string t1) = (Label.to_string t2));
        OUnit2.assert_equal false ((Label.to_string t2) = (Label.to_string t3));
        OUnit2.assert_equal false ((Label.to_string t3) = (Label.to_string t1));
      );

    OUnit2.(>::) "test_create_native" (fun _ ->
        let manager = Label.init_manager in
        let manager, t1 = Label.gen manager "s1" in
        let _, t2 = Label.gen_and_bind manager "s1" in
        let foo_t = Label.create_native "foo" in
        let bar_t = Label.create_native "bar" in
        OUnit2.assert_equal false ((Label.to_string t1) = (Label.to_string foo_t));
        OUnit2.assert_equal false ((Label.to_string t2) = (Label.to_string foo_t));
        OUnit2.assert_equal false ((Label.to_string t1) = (Label.to_string bar_t));
        OUnit2.assert_equal false ((Label.to_string t2) = (Label.to_string bar_t));
        OUnit2.assert_equal false ((Label.to_string foo_t) = (Label.to_string bar_t));
      );

    (* test with [get] together *)
    OUnit2.(>::) "test_gen_and_bind" (fun _ ->
        let manager = Label.init_manager in
        let s1, s2 = "s1", "s2" in
        let manager, t1 = Label.gen_and_bind manager s1 in
        let manager, t2 = Label.gen_and_bind manager s2 in
        let manager, t3 = Label.gen manager "s1" in
        let manager, t4 = Label.gen manager "s2" in
        OUnit2.assert_equal false ((Label.to_string t1) = (Label.to_string t2));
        OUnit2.assert_equal false ((Label.to_string t2) = (Label.to_string t3));
        OUnit2.assert_equal false ((Label.to_string t3) = (Label.to_string t4));
        OUnit2.assert_equal false ((Label.to_string t4) = (Label.to_string t1));
        OUnit2.assert_equal false ((Label.to_string t3) = (Label.to_string t2));
        OUnit2.assert_equal false ((Label.to_string t4) = (Label.to_string t2));
        let t1' = Label.get manager s1 in
        let t2' = Label.get manager s2 in
        OUnit2.assert_equal
          (Some (Label.to_string t1))
          (Option.map Label.to_string t1');
        OUnit2.assert_equal
          (Some (Label.to_string t2))
          (Option.map Label.to_string t2');
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
