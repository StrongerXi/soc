open Pervasives

let tests = OUnit2.(>:::) "filename_test" [

    OUnit2.(>::) "test_split_extension" (fun _ ->
        OUnit2.assert_equal 
          ("", None)
          (Filename.split_extension "");
        OUnit2.assert_equal 
          ("abc", None)
          (Filename.split_extension "abc");
        OUnit2.assert_equal 
          ("", Some "")
          (Filename.split_extension ".");
        OUnit2.assert_equal 
          ("a.", Some "")
          (Filename.split_extension "a..");
        OUnit2.assert_equal 
          ("a.b", Some "cde")
          (Filename.split_extension "a.b.cde");
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
