
let check_and_output_str str ref_path output_path =
  let output = Stdlib.open_out output_path in
  Stdlib.output_string output str;
  Stdlib.close_out output;
  let ref_str = Externals.read_entire_file ref_path in
  OUnit2.assert_equal ref_str str;
;;

let check_set expects actuals =
  OUnit2.assert_equal (List.length expects) (Set.size actuals);
  List.iter
    (fun elem -> 
      OUnit2.assert_equal true (Set.mem elem actuals))
    expects
;;

let check_unordered_list expects actuals =
  OUnit2.assert_equal (List.length expects) (List.length actuals);
  List.iter
    (fun elem -> 
      OUnit2.assert_equal true (List.mem elem actuals))
    expects
;;
