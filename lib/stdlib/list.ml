open Pervasives

let cons x xs = x::xs
;;

let rec fold_left f acc xs =
  match xs with
  | []    -> acc
  | x::xs -> fold_left f (f acc x) xs
;;

let rec fold_right f xs acc =
  match xs with
  | []    -> acc
  | x::xs -> f x (fold_right f xs acc)
;;

let length xs =
  fold_left (fun n _ -> n + 1) 0 xs
;;

let rev xs =
  fold_left (fun reved x -> x::reved) [] xs
;;

let append xs ys =
  fold_right cons xs ys
;;

let concat xss =
  fold_right append xss []
;;

let flatten = concat
;;

let rec map f xs =
  match xs with
  | []    -> []
  | x::xs -> (f x)::(map f xs)
;;

let rec for_all pred xs =
  match xs with
  | []    -> true
  | x::xs -> (pred x) && (for_all pred xs)
;;

let rec exists pred xs =
  match xs with
  | []    -> false
  | x::xs -> (pred x) || (exists pred xs)
;;

let rec mem target xs =
  match xs with
  | []    -> false
  | x::xs -> (target = x) || (mem target xs)
;;

let rec find_opt pred xs =
  match xs with
  | []    -> None
  | x::xs ->
    if pred x then Some x
    else find_opt pred xs
;;

let rec filter pred xs =
  match xs with
  | []    -> []
  | x::xs ->
    if pred x then x::(filter pred xs)
    else filter pred xs
;;

let rec partition pred xs =
  match xs with
  | []    -> [], []
  | x::xs ->
    let yess, nops = partition pred xs in
    if pred x then (x::yess, nops)
    else (yess, x::nops)
;;

let rec assoc_opt key pairs =
  match pairs with
  | [] -> None
  | (k, v)::pairs ->
    if k = key then Some v
    else assoc_opt key pairs
;;

let rec remove_assoc key pairs =
  match pairs with
  | [] -> []
  | (k, v)::pairs ->
    if k = key then remove_assoc key pairs
    else (k, v)::(remove_assoc key pairs)
;;

let remove_dups are_dups xs =
  let rec go (rest : 'a list) (uniques : 'a list) : 'a list =
    match rest with
    | [] -> rev uniques
    | elem::rest ->
      (match find_opt (are_dups elem) uniques with
       | None   -> go rest (elem::uniques)
       | Some _ -> go rest uniques)
  in
  go xs []
;;

let rec split xys =
  match xys with
  | [] -> [], []
  | (x, y)::xys ->
    let xs, ys = split xys in
    (x::xs, y::ys)
;;