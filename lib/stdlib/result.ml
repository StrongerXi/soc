open Pervasives

let ok x = Ok x
;;

let error x = Error x
;;

let bind res f =
  match res with
  | Ok v     -> f v
  | Error v  -> Error v
;;

let join res =
  bind res (fun x -> x)
;;

let map f res =
  match res with
  | Ok v    -> Ok(f v)
  | Error v -> Error v
;;
