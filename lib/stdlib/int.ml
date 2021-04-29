
let min x y =
  if x < y then x else y
;;

let max x y =
  if x > y then x else y
;;

let ceil_div n1 n2 =
  (n1 + n2 - 1) / n2
;;

let offset_to_align n alignment =
  let extra = n mod alignment in
  if extra = 0 then 0
  else alignment - extra
;;

let compare n1 n2 =
  if n1 = n2 then 0
  else if n1 > n2 then 1
  else -1
;;

let to_string = Externals.int_to_string
;;
