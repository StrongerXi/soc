open Pervasives

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

(** [check_set expects actuals] makes sure
    - [expects] and [actuals] contain the same # of elements
    - All elements from [expects] are in [actuals] *)
let check_set (expects : 'a list) (actuals : 'a Set.t) : unit =
  OUnit2.assert_equal (List.length expects) (Set.size actuals);
  List.iter
    (fun elem -> 
      OUnit2.assert_equal true (Set.mem elem actuals))
    expects
;;

(** [check_set expects actuals] makes sure
    - [expects] and [actuals] contain the same # of elements
    - All elements from [expects] are in [actuals] 
    NOTE intentionally separated from [check_set] because I want to use the
    right [mem] function for testing. *)
let check_unordered_list (expects : 'a list) (actuals : 'a list) : unit =
  OUnit2.assert_equal (List.length expects) (List.length actuals);
  List.iter
    (fun elem -> 
      OUnit2.assert_equal true (List.mem elem actuals))
    expects
;;

(* TODO move to list module *)
let rec list_equal (cmp : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list)
  : bool =
  match l1, l2 with
  | [], [] -> true
  | [], _  -> false
  | _, []  -> false
  | x1::l1, x2::l2 ->
    (cmp x1 x2) && (list_equal cmp l1 l2)
;;
