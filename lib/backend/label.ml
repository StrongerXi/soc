
(* [t] = "{name}_{suffix}" *)
type t =
  { name : string
  ; suffix : string
  }

(* Uniqueness of created [t] guaranteed by uniqueness of [stamp] *)
let _create_t_with_stamp (name : string) (stamp : int) : t =
  let suffix = Int.to_string stamp in
  { name; suffix }
;;

let to_string t = String.join_with [t.name; t.suffix] "_"
;;

let _add_suffix t (extra : string) : t =
  let suffix = String.append t.suffix extra in
  { t with suffix }
;;

(* NOTE this must synch with the assembly label of the externally defined
 * function. *)
let get_native (name : string) =
  { name; suffix = "$" } (* disjoint from other [t] *)
;;

let to_epilogue t =
  let suffix = String.append t.suffix "$epilogue" in
  { t with suffix } (* disjoint from other [t] *)
;;


type manager =
  { next_stamp : int
  ; name_cache : (string, t) Map.t
  }

let init_manager =
  { next_stamp = 0
  ; name_cache = Map.empty String.compare
  }
;;

let _gen_with_prefix manager (prefix : string) : (manager * t) = 
  let stamp = manager.next_stamp in
  let t = _create_t_with_stamp prefix stamp in
  let next_stamp = stamp + 1 in
  let manager = { manager with next_stamp } in
  (manager, t)
;;

let gen manager prefix = 
  _gen_with_prefix manager prefix
;;

let gen_and_bind manager (s : string) : (manager * t) =
  let manager, t = _gen_with_prefix manager s in
  let name_cache = Map.add s t manager.name_cache in
  let manager = { manager with name_cache } in
  (manager, t)
;;

let get manager s =
  Map.get s manager.name_cache
;;
