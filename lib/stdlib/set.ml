open Pervasives

(** Currently represented as a list, where uniqueness of elements is maintained
    lazily (i.e., only when needed/observed, like in [size t].
    This is to optimize [add] for now. *)
type 'a t =
  { cmp : 'a -> 'a -> int
  ; rep : 'a list
  }


let _equal_by_cmp (cmp : 'a -> 'a -> int) (e1 : 'a) (e2 : 'a) : bool =
  (cmp e1 e2) = 0
;;

let _unique_elems t =
  List.remove_dups (_equal_by_cmp t.cmp) t.rep
;;


let empty cmp =
  { cmp; rep = [] }
;;

let is_empty t =
  t.rep = []
;;

let add e t =
  { t with rep = e::t.rep }
;;

let remove e t =
  { t with rep = List.filter (fun x -> not (_equal_by_cmp t.cmp e x)) t.rep }
;;

let mem e t =
  match List.find_opt (_equal_by_cmp t.cmp e) t.rep with
  | None   -> false
  | Some _ -> true
;;

let size t =
  List.length (_unique_elems t)
;;

let map f t =
  { t with rep = List.map f t.rep }
;;

let union t1 t2 =
  let rep = List.append t1.rep t2.rep in
  { t1 with rep }
;;

let inter t1 t2 =
  let rep = List.filter (fun e -> mem e t2) t1.rep in
  { t1 with rep }
;;

let disjoint t1 t2 =
  (inter t1 t2).rep = []
;;

let diff t1 t2 =
  let rep = List.filter (fun e -> not (mem e t2)) t1.rep in
  { t1 with rep }
;;

let subset t1 t2 =
  List.for_all (fun e -> mem e t2) t1.rep
;;

let to_list t =
  List.remove_dups (_equal_by_cmp t.cmp) t.rep
;;

let to_string f t =
  let es = List.map f (_unique_elems t) in
  let inner = String.join_with es "; " in
  String.append "{" (String.append inner "}")
;;
