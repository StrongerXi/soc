
type t =
  { row : int
  ; col : int
  }

let create row col =
  { row; col }
;;

let to_string t =
  let str = "(" in
  let str = String.append str (Int.to_string t.row) in
  let str = String.append str ", " in
  let str = String.append str (Int.to_string t.col) in
  let str = String.append str ")" in
  str
;;

let advance t =
  { t with col = t.col + 1 }
;;

let skip_line t =
  { row = t.row + 1; col = 0 }
;;
