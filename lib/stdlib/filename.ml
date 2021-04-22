open Pervasives

(* ASSUME [n < String.length s]
 * Excludes [String.get s n], return the substring before and after it. *)
let _split_at (s : string) (n : int) =
  let s_length = String.length s in
  let before_n = String.sub s 0 n in
  let after_n  = String.sub s (n + 1) (s_length - n - 1) in
  (before_n, after_n)
;;

let split_extension s =
  let rec go n = (* ASSUME n < [s_length] *)
    if n < 0 then (s, None)
    else
      match String.get s n with
      | '.' ->
        let before_dot, after_dot = _split_at s n in
        (before_dot, Some after_dot)
      | _ -> go (n - 1)
  in
  go ((String.length s) - 1)
;;
