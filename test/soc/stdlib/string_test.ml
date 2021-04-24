
let tests = OUnit2.(>:::) "string_test" [
    OUnit2.(>::) "test_length" (fun _ ->
        OUnit2.assert_equal 0 (String.length "");
        OUnit2.assert_equal 1 (String.length "$");
        OUnit2.assert_equal 5 (String.length "5a#2-");
      );

    OUnit2.(>::) "test_get" (fun _ ->
        OUnit2.assert_equal 'a' (String.get "ax$" 0);
        OUnit2.assert_equal '$' (String.get "x2$k" 2);
      );

    OUnit2.(>::) "test_sub" (fun _ ->
        OUnit2.assert_equal "a" (String.sub "ax$" 0 1);
        OUnit2.assert_equal "ax" (String.sub "ax$" 0 2);
        OUnit2.assert_equal "ax$" (String.sub "ax$" 0 3);
        OUnit2.assert_equal "x$" (String.sub "ax$" 1 2);
        OUnit2.assert_equal "$" (String.sub "ax$" 2 1);
      );

    OUnit2.(>::) "test_append" (fun _ ->
        OUnit2.assert_equal "" (String.append "" "");
        OUnit2.assert_equal "123" (String.append "" "123");
        OUnit2.assert_equal "abc" (String.append "abc" "");
        OUnit2.assert_equal "fo-obar" (String.append "fo" "-obar");
      );

    OUnit2.(>::) "test_compare" (fun _ ->
        OUnit2.assert_equal true ((String.compare "ab0" "ab0") = 0);
        OUnit2.assert_equal true ((String.compare "ab" "ab0") < 0);
        OUnit2.assert_equal true ((String.compare "ab0" "ab") > 0);
        OUnit2.assert_equal true ((String.compare "z" "ab0") > 0);
      );

    OUnit2.(>::) "test_join_with" (fun _ ->
        OUnit2.assert_equal "" (String.join_with [] "");
        OUnit2.assert_equal "" (String.join_with [] "aaa");
        OUnit2.assert_equal "ss" (String.join_with ["ss"] "aaa");
        OUnit2.assert_equal "a; bb; a" (String.join_with ["a"; "bb"; "a"] "; ");
      );

    OUnit2.(>::) "test_concat" (fun _ ->
        OUnit2.assert_equal "" (String.concat []);
        OUnit2.assert_equal "a cc" (String.concat ["a"; " "; "cc"]);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
