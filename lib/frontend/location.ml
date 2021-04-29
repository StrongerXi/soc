

type t =
  { row : int
  ; col : int
  }

let create row col =
  { row; col }
;;

let to_string t =
  let row_str, col_str = Int.to_string t.row, Int.to_string t.col in
  "(" ^ row_str ^ ", " ^ col_str ^ ")"
;;

let advance t =
  { t with col = t.col + 1 }
;;

let skip_line t =
  { row = t.row + 1; col = 1 }
;;
