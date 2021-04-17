open Pervasives

(** Currently represented as an association list, where uniqueness of keys is
    maintained lazily (i.e., only when needed/observed, like in [size t].  This
    is to optimize [add] for now. *)
type ('k, 'v) t =
  { cmp : 'k -> 'k -> int
  ; rep : ('k * 'v) list
  }


let _equal_by_cmp
    (cmp : 'a -> 'a -> int)
    ((e1, _) : ('a * 'b))
    ((e2, _) : ('a * 'c))
  : bool =
  (cmp e1 e2) = 0
;;

let _has_key_by_cmp
    (cmp : 'a -> 'a -> int)
    (k : 'a)
    ((k1, _) : ('a * 'b))
  : bool =
  (cmp k k1) = 0
;;

let _unique_pairs t =
  List.remove_dups (_equal_by_cmp t.cmp) t.rep
;;


let empty cmp =
  { cmp; rep = [] }
;;

let is_empty t =
  t.rep = []
;;

let size t =
  List.length (_unique_pairs t)
;;

let get k t =
  let opt_pair = List.find_opt (_has_key_by_cmp t.cmp k) t.rep in
  Option.map (fun (_, v) -> v) opt_pair
;;

let add k v t =
  { t with rep = (k, v)::t.rep }
;;

let remove k t =
  { t with rep = List.filter (fun x -> not (_has_key_by_cmp t.cmp k x)) t.rep }
;;

let map f t =
  let f (k, v) = (k, f v) in
  { t with rep = List.map f t.rep }
;;

let mapi f t =
  let f (k, v) = (k, f k v) in
  { t with rep = List.map f t.rep }
;;

let fold f t a =
  let unique_pairs = List.remove_dups (_equal_by_cmp t.cmp) t.rep in
  let unique_vals = List.map (fun (_, v) -> v) unique_pairs in
  List.fold_right f unique_vals a
;;

let foldi f t acc =
  let unique_vals = List.remove_dups (_equal_by_cmp t.cmp) t.rep in
  List.fold_left (fun acc (k, v) -> f k v acc) acc unique_vals
;;

let to_string f g t =
  let pair_to_str (k, v) =
    String.join_with ["("; (f k); ", "; (g v); ")"] ""
  in
  let pairs = List.map pair_to_str (_unique_pairs t) in
  let inner = String.join_with pairs "; " in
  String.append "{" (String.append inner "}")
;;

let get_key_set t =
  List.fold_right 
    (fun (key, _) key_set -> Set.add key key_set)
    (_unique_pairs t)
    (Set.empty t.cmp)
;;
