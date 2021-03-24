
(** Is [ch] within [ch1, ch2]? *)
let _between ch low_ch high_ch =
  let code = Externals.char_code ch in
  let low = Externals.char_code low_ch in
  let high = Externals.char_code high_ch in
  (low <= code) && (code <= high)
;;


let is_uppercase ch =
  _between ch 'A' 'Z'
;;

let is_lowercase ch =
  _between ch 'a' 'z'
;;

let is_alpha ch =
  (is_uppercase ch) || (is_lowercase ch)
;;

let is_num ch =
  _between ch '0' '9'
;;

let is_alphanum ch =
  (is_alpha ch) || (is_num ch)
;;

let compare ch1 ch2 =
  let code1 = Externals.char_code ch1 in
  let code2 = Externals.char_code ch2 in
  code1 - code2 (* these can't overflow *)
;;
