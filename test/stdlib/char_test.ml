open Pervasives

let tests = OUnit2.(>:::) "char_test" [
    OUnit2.(>::) "test_is_uppercase" (fun _ ->
        OUnit2.assert_equal true (Char.is_uppercase 'A');
        OUnit2.assert_equal true (Char.is_uppercase 'C');
        OUnit2.assert_equal true (Char.is_uppercase 'Z');
        OUnit2.assert_equal false (Char.is_uppercase '4');
        OUnit2.assert_equal false (Char.is_uppercase '-');
        OUnit2.assert_equal false (Char.is_uppercase 'k');
      );

    OUnit2.(>::) "test_is_lowercase" (fun _ ->
        OUnit2.assert_equal true (Char.is_lowercase 'a');
        OUnit2.assert_equal true (Char.is_lowercase 'g');
        OUnit2.assert_equal true (Char.is_lowercase 'z');
        OUnit2.assert_equal false (Char.is_lowercase '4');
        OUnit2.assert_equal false (Char.is_lowercase '-');
        OUnit2.assert_equal false (Char.is_lowercase 'C');
      );

    OUnit2.(>::) "test_is_alpha" (fun _ ->
        OUnit2.assert_equal true (Char.is_alpha 'a');
        OUnit2.assert_equal true (Char.is_alpha 'g');
        OUnit2.assert_equal true (Char.is_alpha 'z');
        OUnit2.assert_equal true (Char.is_alpha 'C');
        OUnit2.assert_equal false (Char.is_alpha '4');
        OUnit2.assert_equal false (Char.is_alpha '-');
        OUnit2.assert_equal false (Char.is_alpha '$');
      );

    OUnit2.(>::) "test_is_num" (fun _ ->
        OUnit2.assert_equal true (Char.is_num '0');
        OUnit2.assert_equal true (Char.is_num '4');
        OUnit2.assert_equal true (Char.is_num '9');
        OUnit2.assert_equal false (Char.is_num 'a');
        OUnit2.assert_equal false (Char.is_num 'g');
        OUnit2.assert_equal false (Char.is_num 'z');
        OUnit2.assert_equal false (Char.is_num 'C');
        OUnit2.assert_equal false (Char.is_num '-');
        OUnit2.assert_equal false (Char.is_num '$');
      );

    OUnit2.(>::) "test_is_alphanum" (fun _ ->
        OUnit2.assert_equal true (Char.is_alphanum '0');
        OUnit2.assert_equal true (Char.is_alphanum '4');
        OUnit2.assert_equal true (Char.is_alphanum '9');
        OUnit2.assert_equal true (Char.is_alphanum 'a');
        OUnit2.assert_equal true (Char.is_alphanum 'g');
        OUnit2.assert_equal true (Char.is_alphanum 'z');
        OUnit2.assert_equal true (Char.is_alphanum 'C');
        OUnit2.assert_equal false (Char.is_alphanum '-');
        OUnit2.assert_equal false (Char.is_alphanum '$');
      );

    OUnit2.(>::) "test_compare" (fun _ ->
        OUnit2.assert_equal true ((Char.compare 'a' 't') < 0);
        OUnit2.assert_equal true ((Char.compare '$' '$') = 0);
        OUnit2.assert_equal true ((Char.compare '5' '2') > 0);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
