
(** [check_and_output_str str ref_path output_path] checks whether [str] matches
    the content of the file at [ref_path], and (over)writes [str] to
    [output_path] *)
let check_and_output_str str ref_path output_path =
  let output = Stdlib.open_out output_path in
  Stdlib.output_string output str;
  Stdlib.close_out output;
  let ref_str = Externals.read_entire_file ref_path in
  OUnit2.assert_equal ref_str str;
;;
