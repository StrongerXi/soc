open Pervasives

let length = Externals.string_length
;;

let get = Externals.string_get
;;

let sub = Externals.string_sub
;;

let append = Externals.string_append
;;

let compare s1 s2 =
  let s1_len, s2_len = (length s1), (length s2) in
  let min_length = Int.min s1_len s2_len in
  let rec go i =
    if i = min_length
    then Int.compare s1_len s2_len
    else
      let cmp = Char.compare (get s1 i) (get s2 i) in
      if cmp = 0 then go (i + 1)
      else cmp
  in
  go 0
;;

let join_with init_ss sep =
  match init_ss with
  | [] -> ""
  | fst_s::rst_ss ->
    let rec go acc ss =
      match ss with
      | [] -> acc
      | s::ss ->
        let acc = append (append acc sep) s in
        go acc ss
    in go fst_s rst_ss
;;
