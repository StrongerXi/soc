
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
