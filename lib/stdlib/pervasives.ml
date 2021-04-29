
type 'a list = 
  | []
  | (::) of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

let not b =
  if b then false else true
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
