
let min x y =
  if x < y then x else y
;;

let max x y =
  if x > y then x else y
;;

let compare n1 n2 =
  if n1 = n2 then 0
  else if n1 > n2 then 1
  else -1
;;

let to_string = Externals.int_to_string
;;

let of_string_opt = Externals.int_of_string_opt
;;
