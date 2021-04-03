
let loc24 = Location.create 2 4
let loc36 = Location.create 3 6
let loc47 = Location.create 4 7
let loc59 = Location.create 5 9

let tests = OUnit2.(>:::) "span_test" [

    OUnit2.(>::) "test_create" (fun _ ->
        OUnit2.assert_equal 
          { Span.start = loc24; final = loc47; }
          (Span.create loc24 loc47);
      );

    OUnit2.(>::) "test_merge" (fun _ ->
        let span_24_36 = Span.create loc24 loc36 in
        let span_24_47 = Span.create loc24 loc47 in
        let span_36_59 = Span.create loc36 loc59 in
        let span_47_59 = Span.create loc47 loc59 in
        OUnit2.assert_equal (* merge non-overlapping span *)
          { Span.start = loc24; final = loc59; }
          (Span.merge span_24_36 span_47_59);
        OUnit2.assert_equal (* merge overlapping span *)
          { Span.start = loc24; final = loc59; }
          (Span.merge span_24_47 span_36_59);
        OUnit2.assert_equal (* merge overlapping span *)
          { Span.start = loc36; final = loc59; }
          (Span.merge span_36_59 span_47_59);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
