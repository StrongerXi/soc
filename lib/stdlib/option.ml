open Pervasives

let some x = Some x
;;

let value opt dft =
  match opt with
  | None   -> dft
  | Some v -> v
;;

let bind opt f =
  match opt with
  | None   -> None
  | Some v -> f v
;;

let join oopt =
  bind oopt (fun x -> x)
;;

let map f opt =
  match opt with
  | None   -> None
  | Some v -> Some(f v)
;;

let iter f opt =
  match opt with
  | None   -> ()
  | Some v -> f v
;;
