
type 'a list         = 'a Externals.list =
  | []
  | (::) of 'a * 'a list

type 'a option       = 'a Externals.option =
  | None
  | Some of 'a

type ('a, 'e) result = ('a, 'e) Externals.result =
  | Ok of 'a
  | Error of 'e

let not b =
  if b then false else true
;;

let int_of_string_opt =
  Externals.int_of_string_opt
;;

let (|>) a f =
  f a
;;

let (@@) f a =
  f a
;;

let rec (@) xs ys =
  match xs with
  | [] -> ys
  | x::xs -> x::(xs @ ys)
;;

let (^) =
  Externals.string_append
;;
